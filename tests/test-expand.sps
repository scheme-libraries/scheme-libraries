#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries reading annotated-datums)
  (scheme-libraries syntax core-environment)
  (scheme-libraries syntax syntax-objects)
  (scheme-libraries syntax expand))

(define expand-datum
  (lambda (x)
    (expand (datum->annotated-datum x)
            core-environment)))

(test-begin "expand")

(test-equal ''10 (expand-datum 10))

(test-equal '(lambda (x.0) 10)
  (expand-datum '(lambda (x) 10)))

(test-end "expand")
