#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library ($syntax)
  (export
    endianness
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
    record-constructor-descriptor
    define-enumeration)
  (import
    ($system))

  ;; (rnrs bytevectors)

  (define-syntax endianness
    (let ([enum-set (make-enumeration (cons (native-endianness) '(big little)))])
      (lambda (stx)
        (syntax-case stx ()
          [(_ symbol)
           (identifier? #'symbol)
           (begin
             (unless (enum-set-member? (syntax->datum #'symbol) enum-set)
               (syntax-violation 'endianness "invalid endianness" stx #'symbol))
             #''symbol)]
          [_ (syntax-violation 'endianness "invalid syntax" stx)]))))

  ;; (rnrs records syntactic)

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

  (define-syntax define-record-type
    (lambda (x)
      (define who 'define-record-type)
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
      (syntax-case x ()
        [(_ name-spec record-clause ...)
         (let-values ([(record-name constructor-name predicate-name)
                       (parse-name-spec #'name-spec)])
           (with-syntax ([record-name record-name]
                         [constructor-name constructor-name]
                         [predicate-name predicate-name])
             #'(begin
                 (define rtd
                   (make-record-type-descriptor
                    'record-name
                    #f                    ;parent
                    #f                    ;uid
                    #f                    ;sealed?
                    #f                    ;opaque?
                    '#()                  ;fields
                    ))
                 (define rcd
                   (make-record-constructor-descriptor
                    rtd
                    #f                    ;parent cd
                    #f                    ;protocol
                    ))
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
                 ;; TODO: parse record fields
                 )))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax record-type-descriptor
    (lambda (x)
      (define who 'record-type-descriptor)
      (syntax-case x ()
        [(_ name)
         (identifier? #'name)
         (lambda (lookup)
           (or (lookup #'name #'record-type-descriptor-key)
               (syntax-violation who "invalid record name syntax" x #'name)))]
        [_
         (syntax-violation who "invalid syntax" x)])))

  (define-syntax record-constructor-descriptor
    (lambda (x)
      (define who 'record-constructor-descriptor)
      (syntax-case x ()
        [(_ name)
         (identifier? #'name)
         (lambda (lookup)
           (or (lookup #'name #'record-constructor-descriptor-key)
               (syntax-violation who "invalid record name syntax" x #'name)))]
        [_ (syntax-violation who "invalid syntax" x)])))

  ;; (rnrs enums)
  (define-syntax define-enumeration
    (lambda (stx)
      (syntax-case stx ()
        [(_ type-name (symbol ...) constructor-name)
         (for-all identifier? #'(typename symbol ... constructor-name))
         #'(begin
             (define universe (make-enumeration '(symbol ...)))
             (define constructor (enum-set-constructor universe))
             (define-syntax type-name
               (let ([enum-set (make-enumeration '(symbol ...))])
                 (lambda (stx)
                   (syntax-case stx ()
                     [(_ x)
                      (identifier? #'x)
                      (let ([e (syntax->datum #'x)])
                        (unless (enum-set-member? e enum-set)
                          (syntax-violation 'type-name "universe does not include specified symbol" stx x))
                        #''x)]
                     [_ (syntax-violation 'type-name "invalid syntax" stx)]))))
             (define-syntax constructor-name
               (let ([enum-set (make-enumeration '(symbol ...))])
                 (lambda (stx)
                   (syntax-case stx ()
                     [(_ x (... ...))
                      (for-all? identifier? #'(x (... ...)))
                      (begin
                        (for-each
                          (lambda (x)
                            (unless (enum-set-member? (syntax->datum x) enum-set)
                              (syntax-violation 'type-name "universe does not include specified symbol" stx x)))
                          #'(x (... ...)))
                        #'(constructor '(x (... ...))))])))))]
        [_ (syntax-violation 'define-enumeration "invalid syntax" stx)]))))
