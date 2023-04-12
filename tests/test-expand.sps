#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries reading annotated-datums)
  (scheme-libraries syntax core-environment)
  (scheme-libraries syntax expressions)
  (scheme-libraries syntax expand)
  (scheme-libraries syntax syntax-objects))

(define expand-datum
  (lambda (x)
    (expand (datum->annotated-datum x)
            (core-environment))))

(define-syntax test-expand
  (syntax-rules ()
    [(test-expand expected source)
     (test-assert (expression=? `expected (expand-datum `source)))]))

(test-begin "expand")

(test-expand (quote 10) 10)

(test-expand (lambda (x1) x1)
  (lambda (x) x))

(test-end "expand")
