#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library ($syntax)
  (export
    endianness
    define-enumeration)
  (import
    ($system)
    (scheme-libraries define-who))

  ;; (rnrs bytevectors)

  (define-syntax/who endianness
    (let ([enum-set (make-enumeration (cons (native-endianness) '(big little)))])
      (lambda (stx)
        (syntax-case stx ()
          [(_ symbol)
           (identifier? #'symbol)
           (begin
             (unless (enum-set-member? (syntax->datum #'symbol) enum-set)
               (syntax-violation 'who "invalid endianness" stx #'symbol))
             #''symbol)]
          [_ (syntax-violation 'who "invalid syntax" stx)]))))

  ;; (rnrs enums)
  (define-syntax/who define-enumeration
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
        [_ (syntax-violation who "invalid syntax" stx)]))))
