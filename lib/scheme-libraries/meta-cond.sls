#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries meta-cond)
  (export
    meta-cond)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define-syntax/who meta-cond
    (lambda (stx)
      (syntax-case stx ()
        [(_ cl1 cl2 ...)
         (with-syntax ([((t e ...) ...) #'(cl1 cl2 ...)])
           #'(let-syntax ([expr (cond
                                 [t (identifier-syntax (begin (values) e ...))]
                                 ...)])
               expr))]
        [_ (syntax-violation who "invalid syntax" stx)]))))
