#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2024).

(library (scheme-libraries macros)
  (export
    cons-if
    syntactify)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries with-implicit))

  (define-syntax/who cons-if
    (lambda (stx)
      (syntax-case stx ()
        [(_ test head tail)
         #'(let ([e tail])
             (if test (cons head tail) tail))]
        [_ (syntax-violation who "invalid syntax" stx)])))

  (define-syntax/who syntactify
    (lambda (stx)
      (syntax-case stx ()
        [(k e) #'(datum->syntax #'k e)]
        [_ (syntax-violation who "invalid syntax" stx)])))
  )
