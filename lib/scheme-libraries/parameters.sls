#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries parameters)
  (export
    make-parameter)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define/who make-parameter
    (case-lambda
      [(init guard)
       (unless (procedure? guard)
         (assertion-violation who "invalid guard argument" guard))
       (let ([v (guard init)])
         (case-lambda
           [() v]
           [(u) (set! v (guard u))]))]
      [(init)
       (make-parameter init values)]))

  )
