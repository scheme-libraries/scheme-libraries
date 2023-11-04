#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax system)
  (export system)
  (import (rnrs base))
  (define system
    (lambda ()
      'scheme-libraries)))
