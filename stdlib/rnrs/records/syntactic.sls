#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs records syntactic (6))
  (export
    define-record-type
    fields
    mutable
    immutable
    parent
    protocol
    sealed
    opaque
    nongenerative
    parent-rtd
    record-type-descriptor
    record-constructor-descriptor)
  (import
    ($system)
    (scheme-libraries with-implicit)
    (scheme-libraries define-who)
    (scheme-libraries syntactic-monads))

  (define-auxiliary-syntax fields)
  (define-auxiliary-syntax mutable)
  (define-auxiliary-syntax immutable)
  (define-auxiliary-syntax parent)
  (define-auxiliary-syntax protocol)
  (define-auxiliary-syntax sealed)
  (define-auxiliary-syntax opaque)
  (define-auxiliary-syntax nongenerative)
  (define-auxiliary-syntax parent-rtd)

  (define record-type-descriptor-key)
  (define record-constructor-descriptor-key)

  (define-syntax/who define-record-type
    (lambda (x)
      (define parse-name-spec
        (lambda (name-spec)
          (syntax-case name-spec ()
            [(record-name constructor-name predicate-name)
             (for-all identifier? #'(record-name constructor-name predicate-name))
             (values #'record-name #'constructor-name #'predicate-name)]
            [record-name
             (identifier? #'record-name)
             (values #'record-name
                     (construct-name #'record-name "make-" #'record-name)
                     (construct-name #'record-name #'record-name "?"))]
            [_ (syntax-violation who "invalid name spec syntax" x name-spec)])))
      (define parse-record-clauses
        (lambda (record-name cl*)
          (define parse-field-spec*
            (lambda (field-spec* definition*)
              (let f ([field-spec* field-spec*]
                      [k 0])
                (cond
                 [(pair? field-spec*)
                  (let*-values ([(field* definition*)
                                 (f (cdr field-spec*) (+ k 1))]
                                [(field definition*)
                                 (parse-field-spec (car field-spec*) definition* k)])
                    (values (cons field field*) definition*))]
                 [(null? field-spec*)
                  (values '() definition*)]
                 [else (assert #f)]))))
          (define parse-field-spec
            (lambda (field-spec definitions k)
              (syntax-case field-spec (mutable immutable)
                [(immutable field-name accessor-name)
                 (and (identifier? #'field-name)
                      (identifier? #'accessor-name))
                 (values #'(immutable field-name)
                         #`((define accessor-name
                              (record-accessor rtd #,k))))]
                [(mutable field-name accessor-name mutator-name)
                 (and (identifier? #'field-name)
                      (identifier? #'accessor-name)
                      (identifier? #'mutator-name))
                 (values #'(mutable field-name)
                         #`((define accessor-name
                              (record-accessor rtd #,k))
                            (define mutator-name
                              (record-mutator rtd #,k))))]
                [(immutable field-name)
                 (identifier? #'field-name)
                 (values #'(immutable field-name)
                         #`((define #,(construct-accessor-name #'field-name)
                              (record-accessor rtd #,k))))]
                [(mutable field-name)
                 (identifier? #'field-name)
                 (values #'(mutable field-name)
                         #`((define #,(construct-accessor-name #'field-name)
                              (record-accessor rtd #,k))
                            (define #,(construct-mutator-name #'field-name)
                              (record-mutator rtd #,k))))]
                [field-name
                 (identifier? #'field-name)
                 (values #'(immutable field-name)
                         #`((define #,(construct-accessor-name #'field-name)
                              (record-accessor rtd #,k))
                            #,@definitions))]
                [_ (syntax-violation who "invalid record field spec" x field-spec)])))
          (define construct-accessor-name
            (lambda (field-name)
              (construct-name field-name record-name "-" field-name)))
          (define construct-mutator-name
            (lambda (field-name)
              (construct-name field-name record-name "-" field-name "-set!")))
          (define-syntactic-monad $ cl*
            field* protocol* sealed* opaque* nongenerative* definition*)
          ($ let f ([field* #f]
                    [protocol* #f]
                    [sealed* #f]
                    [opaque* #f]
                    [nongenerative* #f]
                    [definition* '()])
            (cond
             [(pair? cl*)
              (let ([cl (car cl*)] [cl* (cdr cl*)])
                (define parse-uid*
                  (lambda (uid*)
                    (syntax-case uid* ()
                      [()
                       (datum->syntax #'here (uid (syntax->datum #'record-name)))]
                      [(uid)
                       (identifier? #'uid)
                       #'uid]
                      [_ (syntax-violation "invalid protocol record clause" x cl)])))
                (syntax-case cl (fields protocol)
                  [(fields field-spec ...)
                   (begin
                     (when field*
                       (syntax-violation who "multiple field record clauses" x cl))
                     (let-values ([(field* definition*)
                                   (parse-field-spec* #'(field-spec ...)
                                                      definition*)])
                       ($ f ())))]
                  [(protocol expression)
                   (begin
                     (when protocol*
                       (syntax-violation who "multiple protocol record clauses" x cl))
                     ($ f ([protocol* (list #'expression)])))]
                  [(sealed flag)
                   (boolean? (syntax->datum #'flag))
                   (begin
                     (when sealed*
                       (syntax-violation who "multiple sealed record clauses" x cl))
                     ($ f ([sealed* #'(flag)])))]
                  [(opaque flag)
                   (boolean? (syntax->datum #'flag))
                   (begin
                     (when opaque*
                       (syntax-violation who "multiple opaque record clauses" x cl))
                     ($ f ([opaque* #'(flag)])))]
                  [(nongenerative . uid*)
                   (begin
                     (when nongenerative*
                       (syntax-violation who "multiple nongenerative record clauses" x cl))
                     (let ([uid (parse-uid* #'uid*)])
                       ($ f ([nongenerative* (list uid)]))))]
                  [_ (syntax-violation who "invalid record clause syntax" x cl)]))]
             [(null? cl*)
              (values (or field* '())
                      protocol*
                      sealed*
                      opaque*
                      nongenerative*
                      definition*)]
             [else (assert #f)]))))
      (syntax-case x ()
        [(_ name-spec record-clause ...)
         (let*-values ([(record-name constructor-name predicate-name)
                        (parse-name-spec #'name-spec)]
                       [(field* protocol* sealed* opaque* nongenerative* definition*)
                        (parse-record-clauses record-name #'(record-clause ...))])
           (with-syntax ([record-name record-name]
                         [constructor-name constructor-name]
                         [predicate-name predicate-name]
                         [(field-spec ...) field*]
                         [(definition ...) definition*]
                         [protocol (and protocol* (car protocol*))]
                         [sealed (and sealed* (car sealed*))]
                         [opaque (and opaque* (car opaque*))]
                         [uid (and nongenerative* (car nongenerative*))])
             #'(begin
                 (define rtd
                   (make-record-type-descriptor
                    'record-name
                    #f                    ;parent
                    uid
                    sealed
                    opaque
                    '#(field-spec ...)
                    ))
                 (define rcd
                   (make-record-constructor-descriptor
                    rtd
                    #f                    ;parent cd
                    protocol))
                 (define constructor-name
                   (record-constructor rcd))
                 (define predicate-name
                   (record-predicate rtd))
                 (define-syntax record-name
                   (lambda (stx)
                     (syntax-violation 'record-name "invalid syntax" stx)))
                 (define-property record-name
                   record-type-descriptor-key #'rtd)
                 (define-property record-name
                   record-constructor-descriptor-key #'rcd)
                 definition ...)))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax/who record-type-descriptor
    (lambda (x)
      (syntax-case x ()
        [(_ name)
         (identifier? #'name)
         (lambda (lookup)
           (or (lookup #'name #'record-type-descriptor-key)
               (syntax-violation who "invalid record name syntax" x #'name)))]
        [_
         (syntax-violation who "invalid syntax" x)])))

  (define-syntax/who record-constructor-descriptor
    (lambda (x)
      (syntax-case x ()
        [(_ name)
         (identifier? #'name)
         (lambda (lookup)
           (or (lookup #'name #'record-constructor-descriptor-key)
               (syntax-violation who "invalid record name syntax" x #'name)))]
        [_ (syntax-violation who "invalid syntax" x)])))


  )
