#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries debug)
  (export
    debug)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define-syntax/who debug
    (lambda (stx)
      (syntax-case stx ()
        [(debug . form)
         ;; TODO: Implement and check debug level.
         #'form]
        [_ (syntax-violation who "invalid syntax" stx)]))))
