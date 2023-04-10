#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax syntax-objects)
  (export
    syntax-match)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries syntax syntax-objects))

  (define-syntax/who syntax-match
    (lambda (stx)
      ;; FIXME
      (assert #f)))
  )
