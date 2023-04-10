#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries rec)
  (export
    rec)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define-syntax/who rec
    (lambda (stx)
      (syntax-case stx ()
	[(_ name expr)
	 #'(letrec ([name expr]) name)]
	[_ (syntax-violation who "invalid syntax" stx)]))))
