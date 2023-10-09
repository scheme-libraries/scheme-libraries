#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rename (rnrs)
    (partition rnrs:partition))
  (scheme-libraries testing)
  (scheme-libraries syntactic-monads))

(test-begin "syntactic monads")

(test-equal '(3 2 4)
  (let ()
    (define-syntactic-monad $ a b)
    (letrec ([f ($ lambda (c)
                   ($ list ([a (+ a b)]) c))])
      (f 1 2 4))))

(test-equal '(3 2 4)
  (let ()
    (define-syntactic-monad $ a b)
    (letrec ([f ($ case-lambda
                   [(c)
                    ($ list ([a (+ a b)]) c)])])
      (f 1 2 4))))

(test-equal '(3 2 4)
  (let ()
    (define-syntactic-monad $ a b)
    ($ define (f c)
       ($ list ([a (+ a b)]) c))
    (f 1 2 4)))

(test-equal '(3 2 4)
  (let ()
    (define-syntactic-monad $ a b)
    ($ let f ([a 1] [b 2] [c 4])
       ($ list ([a (+ a b)]) c))))

(define partition
  (lambda (pred ls)
    (define-syntactic-monad $ in out)
    (let f ([ls ls])
      (if (null? ls)
          ($ values ([in '()] [out '()]))
          ($ let*-values ([() (f (cdr ls))])
            (let ([x (car ls)])
              (if (pred x)
                  ($ values ([in (cons x in)]))
                  ($ values ([out (cons x out)])))))))))

(test-equal '((one four five) (2 3 6))
  (let-values ([(in out)
                (partition symbol? '(one 2 3 four five 6))])
    (list in out)))

(define factor
  (lambda (n)
    (define-syntactic-monad $ n i)
    ($ let f ([i 2])
      (cond
        [(>= i n) (list n)]
        [(integer? (/ n i))
         (cons i ($ f [(n (/ n i))]))]
        [else ($ f [(i (+ i 1))])]))))

(test-equal '(2 2 2 2 2 2 2 2 3 3 3 3 5 5 7)
  (factor 3628800))

(test-end "syntactic monads")
