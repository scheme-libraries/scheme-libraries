#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries helpers)
  (export
    symbolic-identifier=?)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define/who symbolic-identifier=?
    (lambda (id1 id2)
      (unless (identifier? id1)
        (assertion-violation who "invalid first identifier argument" id1))
      (unless (identifier? id2)
        (assertion-violation who "invalid second identifier argument" id2))
      (symbol=? (syntax->datum id1)
                (syntax->datum id2))))

  )
