#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries impure)
  (export
    increment!
    prepend!)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define-syntax/who increment!
    (lambda (stx)
      (syntax-case stx ()
        [(_ id)
         (identifier? #'id)
         #'(set! id (+ id 1))]
        [(_ id expr)
         (identifier? #'id)
         #'(set! id (+ id expr))]
        [_ (syntax-violation who "invalid syntax" stx)])))

  (define-syntax/who prepend!
    (lambda (stx)
      (syntax-case stx ()
        [(_ list-id list-expr)
         (identifier? #'list-id)
         #'(set! list-id (append list-expr list-id))]
        [_ (syntax-violation who "invalid syntax" stx)]))))
