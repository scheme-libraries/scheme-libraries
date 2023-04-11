#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries parameters)
  (export
    make-parameter
    parameterize)
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

  (define-syntax parameterize
    (lambda (x)
      (syntax-case x ()
        [(_ () b1 ... b2) #'(begin b1 ... b2)]
        [(_ ([x e] ...) b1 ... b2)
         (with-syntax ([(p ...) (generate-temporaries #'(x ...))]
                       [(y ...) (generate-temporaries #'(x ...))])
           #'(let ([p x] ... [y e] ...)
               (let ([swap (lambda ()
                             (let ([t (p)]) (p y) (set! y t))
                             ...)])
                 (dynamic-wind swap (lambda () b1 ... b2) swap))))])))

  )
