#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2024).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries macros))

(test-begin "macros")

(test-equal '(1 2) (cons-if #t 1 '(2)))
(test-equal '(2) (cons-if #f 1 '(2)))
(test-equal '(2) (cons-if #f (raise 42) '(2)))

(test-end "macros")
