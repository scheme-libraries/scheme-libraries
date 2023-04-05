#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries debug)
  (export
    safe-assert)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define/who safe-assert
    (lambda (x)
      (syntax-case x ()
        [(_ e1 ...)
         #'(begin
             (assert e1) ...
             (values))]
        [_ (syntax-violation who "invalid syntax" x)]))))
