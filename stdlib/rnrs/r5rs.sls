#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs r5rs (6))
  (export
    (rename
      (inexact exact->inexact)
      (exact inexact->exact))
    quotient
    remainder
    modulo
    delay
    force
    ;; null-environment
    ;; scheme-report-environment
    )
  (import
    ($system)
    (scheme-libraries define-who))

  (define-syntax/who delay
    (lambda (x)
      (syntax-case x ()
        [(_ expr) #'($delay (lambda () expr))]
        [_ (syntax-violation who "invalid syntax" x)]))))
