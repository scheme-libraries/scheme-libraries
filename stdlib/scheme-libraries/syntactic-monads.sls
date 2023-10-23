#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntactic-monads)
  (export
    define-syntactic-monad)
  (import
    ($system)
    (scheme-libraries define-who)
    (scheme-libraries with-implicit))

  (define-syntax/who define-syntactic-monad
    (lambda (x)
      (syntax-case x ()
        [(_ name formal ...)
         (for-all identifier? #'(name formal ...))
         #'(define-syntax/who name
             (lambda (stx)
               (syntax-case stx (lambda case-lambda let let*-values define)
                 [(k lambda formals . body)
                  (with-implicit (k formal ...)
                    #'(lambda (formal ... . formals) . body))]
                 [(k case-lambda [formals . body] (... ...))
                  (with-implicit (k formal ...)
                    #'(case-lambda [(formal ... . formals) . body] (... ...)))]
                 [(k define (proc-name . formals) . body)
                  (with-implicit (k formal ...)
                    #'(define (proc-name formal ... . formals) . body))]
                 [(k let*-values ([formals init] (... ...)) . body)
                  (with-implicit (k formal ...)
                    #'(let*-values ([(formal ... . formals) init] (... ...)) . body))]
                 [(k proc-expr ([x e] (... ...)) arg (... ...))
                  (for-all identifier? #'(x (... ...)))
                  (with-implicit (k formal ...)
                    (define state-variable?
                      (lambda (x)
                        (find (lambda (y)
                                (bound-identifier=? x y))
                              #'(formal ...))))
                    (for-each
                      (lambda (x)
                        (unless (state-variable? x)
                          (syntax-violation who "undefined state variable" stx x)))
                      #'(x (... ...)))
                    #'(let ([proc proc-expr])
                        (let ([x e] (... ...))
                          (proc formal ... arg (... ...)))))]
                 [(k proc-expr)
                  (with-implicit (k formal ...)
                    #'(proc-expr formal ...))]
                 [(k let f ([x e] (... ...)) . body)
                  (for-all identifier? #'(f x (... ...)))
                  (with-implicit (k formal ...)
                    (define state-variable?
                      (lambda (x)
                        (find (lambda (y)
                                (bound-identifier=? x y))
                              #'(formal ...))))
                    (let-values ([(bindings more-bindings)
                                  (partition (lambda (binding)
                                               (syntax-case binding ()
                                                 [[x e] (state-variable? #'x)]))
                                             #'([x e] (... ...)))])
                      (with-syntax ([([x e] (... ...)) bindings]
                                    [([more-x more-e] (... ...)) more-bindings])
                        #'(letrec ([f (lambda (formal ... more-x (... ...)) . body)])
                            (let ([proc f])
                              (let ([x e] (... ...))
                                (proc formal ... more-e (... ...))))))))]
                 [_ (syntax-violation who "invalid syntax" stx)])))]
        [_ (syntax-violation who "invalid syntax" x)]))))
