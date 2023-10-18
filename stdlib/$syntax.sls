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

  (define-syntax define-record-type
    (lambda (x)
      ;; FIXME
      #f))

  (define-syntax record-type-descriptor
    (lambda (x)
      ;; FIXME
      #f))

  (define-syntax record-constructor-descriptor
    (lambda (x)
      ;; FIXME
      #f))

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
