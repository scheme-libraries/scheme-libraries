#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries with-implicit)
  (export
    with-implicit)
  (import
    ($system))

  (define-syntax with-implicit
    (lambda (x)
      (syntax-case x ()
        [(_ (k x ...) e1 ... e2)
         #'(with-syntax ([x (datum->syntax #'k 'x)] ...)
             e1 ... e2)]
        [_ (syntax-violation 'with-implicit "invalid syntax" x)]))))
