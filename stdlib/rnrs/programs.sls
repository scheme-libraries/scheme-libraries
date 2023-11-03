#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs programs (6))
  (export
    command-line
    exit)
  (import
    ($system))

  (define command-line
    (lambda ()
      (current-command-line))))
