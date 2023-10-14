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

    (only (chezscheme) trace-define)    ;FIXME DEBUG

    (scheme-libraries define-who)
    (scheme-libraries match)
    (scheme-libraries parameters)
    (scheme-libraries strings)
    (scheme-libraries syntax bootstrap-environment)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax $marks)
    (scheme-libraries syntax $ribs)
    (scheme-libraries syntax variables)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax library-collections)
    (scheme-libraries syntax syntax-objects))

  ;; Library collections

  (trace-define library-collection->datum
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
      (let ([imports (library-imports lib)]
            [viscode (expression->datum (library-visit-code lib))]
            [invcode (expression->datum (library-invoke-code lib))])
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
                                  `(,lblsym (variable ,(variable-name
                                                        (variable-binding-symbol bdg))
                                                      ,(location-name
                                                        (variable-binding-location bdg))))]
                                 [(keyword-binding? bdg)
                                  `(,lblsym (keyword))]
                                 [else (assert #f)]))
                              (library-bindings lib) ;FIXME: Call this environment.
                              ))

             (visit-code ,viscode)
             (invoke-code ,invcode))))))

  (define/who datum->library
    ;; TODO: Check that library names corresponding to uids match.
    (lambda (e)
      (match e
        [($library (,name ... ,version)
           (uid ,uid)
           (import ((,impname** ... ,impver*) ,impuid*) ...)
           (visit-requirements ,visreq* ...)
           (invoke-requirements ,invreq* ...)
           (export ,expexp* ...)
           (environment (,[datum->label -> lbl*] ,type*) ...)
           (visit-code ,viscode)
           (invoke-code ,invcode))
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
                 (datum->expression viscode)
                 ;; Invoke definitions
                 (datum->expression invcode)
                 ;; Bindings
                 lbl*                   ;XXX:store it under env instead?
                 )])
           (for-each
             (lambda (lbl type)
               (match type
                 [(variable ,sym ,locname)
                  (let ([bdg (make-variable-binding (name->variable sym)
                                                    (datum->location locname))])
                    (variable-binding-library-set! bdg lib)
                    (label-binding-set! lbl bdg))]
                 [(keyword)
                  (let ([bdg (make-keyword-binding #f)])
                    (keyword-binding-library-set! bdg lib)
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
        [(quote ,e) `(quote ,e)]
        [,e (guard (variable? e)) (variable->datum e)]
        [,e (guard (symbol? e)) e]
        [(,[e*] ...) e*]
        [,x
         ;; XXX FIXME DEBUG
         (display x) (newline)

         (assert #f)])))

  (define datum->expression
    (lambda (e)
      (match e
        [(quote ,e) `(quote ,e)]
        [,e
         (guard (symbol? e) (location-symbol? e))
         `(quote ,(datum->location e))]
        [,e
         (guard (symbol? e) (label-symbol? e))
         `(quote ,(datum->label e))]
        [,e
         (guard (symbol? e) (mark-symbol? e))
         `(quote ,(datum->mark e))]
        [,e
         (guard (symbol? e) (variable-symbol? e))
         (datum->variable e)]
        [,e (guard (symbol? e)) e]
        [(,[e*] ...) e*]
        [,e (assert #f)])))

  (define location-symbol?
    (lambda (sym)
      (string-prefix? "%location-" (symbol->string sym))))

  (define label-symbol?
    (lambda (sym)
      (string-prefix? "%label-" (symbol->string sym))))

  (define mark-symbol?
    (lambda (sym)
      (string-prefix? "%mark-" (symbol->string sym))))

  (define variable-symbol?
    (lambda (sym)
      (string-prefix? "%variable-" (symbol->string sym))))

  )
