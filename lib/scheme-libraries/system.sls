#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2024).

(library (scheme-libraries system)
  (export system
          system-meta-level)
  (import (rnrs)
          (scheme-libraries define-who))

  (define system
    (lambda ()
      'unknown))

  (define-syntax/who system-meta-level
    (lambda (stx)
      (syntax-case stx ()
        [(k) #'#f]
        [_ (syntax-violation who "invalid syntax" stx)]))))
