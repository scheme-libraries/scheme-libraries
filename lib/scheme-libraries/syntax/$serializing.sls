#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $serializing)
  (export
    library-collection->datum
    datum->library-collection
    library->datum
    datum->library)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries match)
    (scheme-libraries parameters)
    (scheme-libraries strings)
    (scheme-libraries syntax bootstrap-environment)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax $marks)
    (scheme-libraries syntax $ribs)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax library-collections)
    (scheme-libraries syntax syntax-objects))

  ;; Library collections

  (define/who library-collection->datum
    (lambda (lc)
      (parameterize ([current-library-collection lc])
        `($library-collection ,@(map library->datum (library-list))))))

  (define/who datum->library-collection
    (lambda (e)
      (parameterize ([current-library-collection (make-library-collection)])
        (match e
          [($library-collection ,libexp* ...)
           (for-each
             (lambda (e)
               (define lib (datum->library e))
               (library-set! (library-name lib) lib))
             libexp*)]
          [,_ (assert #f)])
        (current-library-collection))))

  ;; Libraries

  (define/who library->datum
    (lambda (lib)
      (assert (library? lib))
      (let ([imports (library-imports lib)])
        (extend-backquote here
          `($library (,@(library-name lib) ,(library-version lib))
             (uid ,(library-uid lib))
             (import ,@(vector->list
                        (vector-map
                         (lambda (implib)
                           `((,@(library-name implib) ,(library-version implib))
                             ,(library-uid implib)))
                         (library-imports lib))))
             (visit-requirements ,@(vector->list
                                    (vector-map library-uid
                                                (library-visit-requirements lib))))
             (invoke-requirements ,@(vector->list
                                     (vector-map library-uid
                                                 (library-invoke-requirements lib))))
             (export ,@(rib-map
                        (lambda (n m l/p)
                          (assert (null? m))
                          `(,n ,(label/props->datum l/p)))
                        (library-exports lib)))
             (environment ,@(map
                              (lambda (lbl)
                                (define bdg (label-binding lbl))
                                (define lblsym (label->datum lbl))
                                (cond
                                 [(variable-binding? bdg)
                                  `(,lblsym (variable ,(variable-binding-symbol bdg)
                                                      ,(location-name
                                                        (variable-binding-location bdg))))]
                                 [(global-keyword-binding? bdg)
                                  `(,lblsym (keyword))]
                                 [else (assert #f)]))
                              (library-bindings lib) ;FIXME: Call this environment.
                              ))
             (visit-code )
             (invoke-code ))))))

  (define/who datum->library
    ;; TODO: Check that library names corresponding to uids match.
    (lambda (e)
      (match e
        [($library (,name ... ,version)
           (uid ,uid)
           (import ((,impname* ,impver*) ,impuid*) ...)
           (visit-requirements ,visreq* ...)
           (invoke-requirements ,invreq* ...)
           (export ,expexp* ...)
           (environment (,[datum->label -> lbl*] ,type*) ...)
           (visit-code ,viscode* ...)
           (invoke-code ,invcode* ...))
         (let ([lib
                (make-library
                 ;; Name
                 name
                 ;; Version
                 version
                 ;; Uid
                 uid
                 ;; Imports
                 (vector-map uid->library (list->vector impuid*))
                 ;; Exports
                 (let ([rib (make-rib)])
                   (for-each
                     (lambda (e)
                       (match e
                         [(,n ,l/p)
                          (rib-set! rib n '() (datum->label/props l/p))]
                         [,_ (assert #f)]))
                     expexp*)
                   rib)
                 ;; Visit requirements
                 (vector-map uid->library (list->vector visreq*))
                 ;; Invoke requirements
                 (vector-map uid->library (list->vector invreq*))
                 ;; Visit commands
                 viscode*                      ;FIXME: link
                 ;; Invoke definitions
                 invcode*                      ;FIXME: link
                 ;; Visiter
                 #f
                 ;; Invoker
                 #f
                 ;; Bindings
                 lbl*                   ;XXX:store it under env instead?
                 )])
           (for-each
             (lambda (lbl type)
               (match type
                 [(variable ,sym ,locname)
                  ;; FIXME: Lookup location for name.

                  ;; XXX: The idea is to use symbol->object from the
                  ;; current collection.  The problem is that only
                  ;; those locations will be interned that will have
                  ;; been previously loaded. We have to do this for
                  ;; all locations in all libraries in a library
                  ;; collection. We can assure this by calculating the
                  ;; serialized version when a library is created.
                  ;; This works if only serialized libraries get a uid. :-]

                  ;; What happens if we serialize a library twice?  ->
                  ;; If we serialize it from different collections,
                  ;; the library will have different label ids.  When
                  ;; do we serialize a library? When we precompile.
                  ;; In principle, we need all previous libs
                  ;; serialized as well. because of uids.


                  (let ([bdg (make-variable-binding sym)])
                    (variable-binding-library-set! bdg lib)
                    (label-binding-set! lbl bdg))]
                 [(keyword)
                    ;; TODO: Get rid of global-keyword-binding
                  (let ([bdg (make-global-keyword-binding #f #f)])
                    (global-keyword-binding-library-set! bdg lib)
                    (label-binding-set! lbl bdg))]))
             lbl* type*)
           lib)]
        [,_
         (assertion-violation who "invalid expanded library definition" e)])))

  ;; Code serialization

  (define expression->datum
    (lambda (e)
      (match e
        [(quote ,loc)
         (guard (location? loc))
         (location->datum loc)]
        [(quote ,mark)
         (guard (mark? mark))
         (mark->datum mark)]
        [(quote ,lbl)
         (guard (label? lbl))
         (label->datum lbl)]
        [(quote ,_) e]
        [(,[rator] ,[rand*] ...)
         `(,rator ,rand* ...)]
        [,_ (assert #f)])))

  (define datum->expression
    (lambda (e)
      (match e
        [(quote ,_) e]
        [,_
         (guard (symbol? e) (location-symbol? e))
         (datum->location e)]
        [,_
         (guard (symbol? e) (label-symbol? e))
         (datum->label e)]
        [,_
         (guard (symbol? e) (mark-symbol? e))
         (datum->mark e)]
        [(,[rator] ,[rand*] ...)
         `(,rator ,rand* ...)]
        [,_ (assert #f)])))

  (define location-symbol?
    (lambda (sym)
      (string-prefix? "location-" (symbol->string sym))))

  (define label-symbol?
    (lambda (sym)
      (string-prefix? "label-" (symbol->string sym))))

  (define mark-symbol?
    (lambda (sym)
      (string-prefix? "mark-" (symbol->string sym))))

  )
