#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2024).

(library (scheme-libraries system)
  (export system
          system-meta-level)
  (import (rnrs)
          (scheme-libraries define-who)
          (only (chezscheme) import $system))

  (import (only $system $tc $tc-field))

  (define system
    (lambda ()
      'chezscheme))

  (define-syntax/who system-meta-level
    (lambda (stx)
      (syntax-case stx ()
        [(k) (datum->syntax #'k ($tc-field 'meta-level ($tc)))]
        [_ (syntax-violation who "invalid syntax" stx)]))))
