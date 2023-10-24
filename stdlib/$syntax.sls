#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library ($syntax)
  (export
    endianness)
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
          [_ (syntax-violation 'who "invalid syntax" stx)])))))
