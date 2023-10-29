#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $serializing)
  (export
    library-collection->datum
    datum->library-collection
    library->datum
    datum->library)
  (import

    ;(prefix (chezscheme) cs:)

    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries hashtables)
    (scheme-libraries match)
    (scheme-libraries parameters)
    (scheme-libraries strings)
    (scheme-libraries uuid)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries syntax bootstrap-environment)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax $marks)
    (scheme-libraries syntax $ribs)
    (scheme-libraries syntax $ribcages)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax variables)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax library-collections)
    (scheme-libraries syntax syntax-objects))

  ;; Library collections

  (define library-collection->datum
    (lambda (lc system? visible?)
      (parameterize ([current-library-collection lc])
        (define libs (library-list))
        (define init-lib (make-init-library (filter system? libs)))
        (define init-uid (library-uid init-lib))
        `($library-collection
          ,(library->datum init-lib #f #f init-uid)
          ,@(map (lambda (lib) (library->datum lib (system? lib) (visible? lib) init-uid)) (library-list))))))

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

  ;; Problem: what happens if one writes out such a library -> the import would

  (define/who library->datum
    (lambda (lib system? visible? init-uid)
      (define object->s-exp
        (lambda (obj)
          (cond
           [(location? obj)
            `(location ,(location->s-exp obj))]
           [(label? obj)
            `(label ,(label->datum obj))]
           [(syntax-object? obj)
            `(syntax ,(syntax-object->s-exp ribcage->symbol obj))]
           [else
            ;; FIXME
            (display obj) (newline)
            (assert #f)])))
      (define ribcages (make-eq-hashtable))
      (define ribcage->symbol
        (lambda (r)
          (hashtable-intern! ribcages
                             r
                             (lambda () (uid 'ribcage)))))
      (assert (library? lib))
      (let ([imports (library-imports lib)]
            [viscode (expression->s-exp object->s-exp (library-visit-code lib))]
            [invcode (expression->s-exp object->s-exp (library-invoke-code lib))])
        (define objects
          (let-values ([(ribcages symbols) (hashtable-entries ribcages)])
            (map
              (lambda (sym ribcage)
                `(,sym . ,(ribcage->s-exp label/props->datum ribcage)))
              (vector->list symbols) (vector->list ribcages))))
        (extend-backquote here
          `($library (,@(if visible?
                            (library-name lib)
                            '(#f))
                      ,(if visible?
                           (library-version lib)
                           '()))
             (uid ,(library-uid lib))
             (import)
             ;; FIXME: for the non-system libraries, we have to retain the imports!
             #;
             (import ,@(vector->list    ; ; ; ; ; ;
             (vector-map                ; ; ; ; ; ;
             (lambda (implib)           ; ; ; ; ; ;
             `((,@(library-name implib) ,(library-version implib)) ; ; ; ; ; ;
             ,(library-uid implib)))    ; ; ; ; ; ;
             (library-imports lib))))
             (visit-requirements ,@(if system?
                                       (list init-uid)
                                       (vector->list
                                        (vector-map (lambda (lib)
                                                      (and lib (library-uid lib)))
                                                    (library-visit-requirements lib)))))
             (invoke-requirements ,@(if system?
                                        (list init-uid)
                                        (vector->list
                                         (vector-map library-uid
                                                     (library-invoke-requirements lib)))))
             (export ,@(rib-map
                        (lambda (n m l/p)
                          (assert (null? m))
                          `(,n ,(label/props->datum l/p)))
                        (library-exports lib)))
             (environment ,@(if system?
                                (list)
                                (map
                                  (lambda (lbl)
                                    (define bdg (label-binding lbl))
                                    (define lblsym (label->datum lbl))
                                    (cond
                                     [(variable-binding? bdg)
                                      `(,lblsym (variable ,(variable->symbol
                                                            (variable-binding-symbol bdg))
                                                          ,(location-name
                                                            (variable-binding-location bdg))))]
                                     [(keyword-binding? bdg)
                                      `(,lblsym (keyword))]
                                     [(property-binding? bdg)
                                      `(,lblsym (property))]
                                     [(auxiliary-binding? bdg)
                                      `(,lblsym (auxiliary-syntax
                                                 ,(auxiliary-binding-name bdg)))]
                                     [else (assert #f)]))
                                  (library-bindings lib)) ;FIXME: Call this environment.
                                ))
             (objects ,objects)
             (visit-code ,(if system?
                              (expression->s-exp object->s-exp
                                                 (build (begin (values))))
                              viscode))
             (invoke-code ,(if system?
                               (expression->s-exp object->s-exp
                                                  (build (letrec ()
                                                           (letrec* ()
                                                             (begin (values))))))
                               invcode)))))))

  (define/who datum->library
    ;; TODO: Check that library names corresponding to uids match.
    (lambda (e)
    (define s-exp->object
      (lambda (e)
        (match e
          [(syntax ,x) (s-exp->syntax-object symbol->ribcage x)]
          [(location ,x) (s-exp->location x)]
          [(label ,x) (datum->label x)]
          [else (assert #f)])))
      (define ribcages (make-eq-hashtable))
      (define intern-objects!
        (lambda (objects)
          (map (lambda (entry)
                 (hashtable-set! ribcages
                                 (car entry)
                                 (s-exp->ribcage datum->label/props (cdr entry))))
               objects)))
      (define symbol->ribcage
        (lambda (r)
          (assert (hashtable-ref ribcages r #f))))
      (match e
        [($library (,name* ... ,version)
           (uid ,uid)
           (import ((,impname** ... ,impver*) ,impuid*) ...)
           (visit-requirements ,visreq* ...)
           (invoke-requirements ,invreq* ...)
           (export ,expexp* ...)
           (environment (,[datum->label -> lbl*] ,type*) ...)
           (objects ,objects)
           (visit-code ,viscode)
           (invoke-code ,invcode))
         (intern-objects! objects)
         (let* ([name (if (equal? name* '(#f)) #f name*)]
                [lib
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
                  (vector-map (lambda (uid)
                                (and uid (uid->library uid)))
                              (list->vector visreq*))
                  ;; Invoke requirements
                  (vector-map uid->library (list->vector invreq*))
                  ;; Visit commands
                  (s-exp->expression s-exp->object viscode)
                  ;; Invoke definitions
                  (s-exp->expression s-exp->object invcode)
                  ;; Bindings
                  lbl*                ;XXX:store it under env instead?
                  )])
           (for-each
             (lambda (lbl type)
               (match type
                 [(variable ,sym ,locname)
                  (let ([bdg (make-variable-binding (symbol->variable sym)
                                                    (s-exp->location locname))])
                    (variable-binding-library-set! bdg lib)
                    (label-binding-set! lbl bdg))]
                 [(keyword)
                  (let ([bdg (make-keyword-binding #f)])
                    (keyword-binding-library-set! bdg lib)
                    (label-binding-set! lbl bdg))]
                 [(property)
                  (let ([bdg (make-property-binding #f)])
                    (property-binding-library-set! bdg lib)
                    (label-binding-set! lbl bdg))]
                 [(auxiliary-syntax ,name)
                  (guard (symbol? name))
                  (make-auxiliary-binding name)]))
             lbl* type*)
           lib)]
        [,_
         (assertion-violation who "invalid expanded library definition" e)])))

  ;; Init library

  (define make-init-library
    (lambda (libs)
      (define name '($init))
      (define version '())
      (define init-uid (uid '$init))
      (define imports '#())
      (define exports (make-rib))
      (define visreqs '#(#f))           ;#f means to invoke itself.
      (define invreqs '#())
      (define viscode
        (build (begin
                 ,(map library-visit-commands libs) ... ...
                 (values))))
      (define invcode
        (build
          (letrec () #;(,(map library-invoke-bindings libs) ... ...)
            (letrec* (,(map library-invoke-definitions libs) ... ...)
              (begin
                ,(map library-invoke-commands libs) ... ...
                (values))))))
      (define env
        (apply append (map library-bindings libs)))

      ;; XXX DEBUG FIXME
      ;; (display "XXXX:\n")
      ;; (display (map library-invoke-commands libs))
      ;; (newline)


      (make-library name version init-uid imports exports visreqs invreqs viscode invcode env)))

  (define library-visit-commands
    (lambda (lib)
      (match (library-visit-code lib)
        [(begin ,viscmd* ... ,body)
         viscmd*])))

  (define library-invoke-bindings
    (lambda (lib)
      (match (library-invoke-code lib)
        [(letrec ,bdg* ,body)
         bdg*])))

  (define library-invoke-definitions
    (lambda (lib)
      (match (library-invoke-code lib)
        [(letrec ,bdg*
           (letrec* ,def* ,body))
         def*])))

  (define library-invoke-commands
    (lambda (lib)
      (match (library-invoke-code lib)
        [(letrec ,bdg*
           (letrec* ,def*
             (begin ,cmd* ... ,body)))
         cmd*])))

  ;; Serializing of syntax objects

  ;; TODO: Many substitutions are composed of the same ribcages.  We
  ;; can compress these lists.  The same may be true for lists of
  ;; marks.

  (define syntax-object->s-exp
    (lambda (ribcage->symbol x)
      (let f ([x x])
        (define expr->s-exp
          (lambda (x)
            (cond
             [(annotated-datum? x)
              `(datum ,(annotated-datum->s-exp x))]
             [(vector? x)
              `(vector ,@(map expr->s-exp (vector->list x)))]
             [(list? x)
              `(list ,@(map expr->s-exp x))]
             [(pair? x)
              (match x
                [(,x* ... . ,x)
                 `(cons* ,@(map expr->s-exp x*) ,(expr->s-exp x))])]
             [(syntax-object? x)
              `(syntax ,(f x))]
             [else x])))
        (define wrap->s-exp
          (lambda (x)
            `#(,(mark-list->s-exp (wrap-marks x))
               ,(substitutions->s-exp (wrap-substitutions x)))))
        (define substitutions->s-exp
          (lambda (s*)
            (map (lambda (s)
                   (substitution->s-exp ribcage->symbol s))
                 s*)))
        `#(,(expr->s-exp (syntax-object-expression x))
           ,(wrap->s-exp (syntax-object-wrap x))))))

  (define s-exp->syntax-object
    (lambda (symbol->ribcage x)
      (let f ([x x])
        (define s-exp->expr
          (lambda (e)
            (match e
              [(datum ,[s-exp->annotated-datum -> x]) x]
              [(vector ,[e*] ...) (list->vector e*)]
              [(list ,[e*] ...) e*]
              [(cons* ,[e*] ... ,[e]) (append e* e)]
              [(syntax ,[f -> x]) x]
              [,x x])))
        (define s-exp->wrap
          (lambda (e)
            (match e
              [#(,[s-exp->mark-list -> x] ,[s-exp->substitutions -> y])
               (make-wrap x y)])))
        (define s-exp->substitutions
          (lambda (e*)
            (map (lambda (e)
                   (s-exp->substitution symbol->ribcage e))
                 e*)))
        (match x
          [#(,[s-exp->expr -> e] ,[s-exp->wrap -> w])
           (make-syntax-object e w)]))))


  )
