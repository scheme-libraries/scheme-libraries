#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries define-auxiliary-syntax)
  (export
    define-auxiliary-syntax)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define-syntax/who define-auxiliary-syntax
    (lambda (x)
      (syntax-case x ()
        [(_ name)
         (identifier? #'name)
         #'(define-syntax/who name
             (lambda (x)
               (syntax-violation who "misplaced auxiliary keyword" x)))]
        [_ (syntax-violation who "invalid syntax" x)]))))
