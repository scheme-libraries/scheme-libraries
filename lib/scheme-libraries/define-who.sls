#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries define-who)
  (export
    define/who
    define-syntax/who)
  (import
    (rnrs)
    (scheme-libraries with-implicit))

  (define-syntax define/who
    (lambda (x)
      (define parse
        (lambda (x)
          (syntax-case x ()
            [(k (f . u) e1 ... e2)
             (identifier? #'f)
             (values #'k #'f #'((lambda u e1 ... e2)))]
            [(k f e1 ... e2)
             (identifier? #'f)
             (values #'k #'f #'(e1 ... e2))]
            [_ (syntax-violation 'define/who "invalid syntax" x)])))
      (let-values ([(k f e*) (parse x)])
        (with-syntax ([k k] [f f] [(e1 ... e2) e*])
          (with-implicit (k who)
            #'(define f
                (let ([who 'f])
                  e1 ... e2)))))))

  (define-syntax define-syntax/who
    (lambda (x)
      (syntax-case x ()
        [(k n e1 ... e2)
         (identifier? #'n)
         (with-implicit (k who)
           #'(define-syntax n
               (let ([who 'n])
                 e1 ... e2)))]))))
