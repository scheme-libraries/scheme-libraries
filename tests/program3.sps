#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs base)
  (rnrs io simple)                      ;XXX
  (test))

(assert (eq? (foo) 12))

(assert (eq? (fruit) 'cherry))
