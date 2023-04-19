#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries syntax bootstrap-environment)
  (scheme-libraries syntax eval)
  (scheme-libraries syntax syntax-objects))

(test-begin "eval")

(test-equal 4 (eval '4 (bootstrap-environment)))

(test-equal '(1 2 3) (eval '`(1 2 ,3) (bootstrap-environment)))
(test-equal '(1 2 3) (eval '`(1 2 ,'3) (bootstrap-environment)))

(test-equal 1 (eval 1 (environment '($system))))

(test-end "eval")
