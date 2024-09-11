#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2024).

(library (scheme-libraries macros)
  (export
    cons-if)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define-syntax/who cons-if
    (lambda (stx)
      (syntax-case stx ()
        [(_ test head tail)
         #'(let ([e tail])
             (if test (cons head tail) tail))]
        [_ (syntax-violation who "invalid syntax" stx)]))))
