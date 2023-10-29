#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs base)
  (rnrs io simple)                      ;XXX
  (test))

;;; XXX Debug
(display (foo))
(newline)

(assert (eq? (foo) 12))

;;; XXX Debug
(display fruit)
(newline)

(assert (eq? fruit 'cherry))
