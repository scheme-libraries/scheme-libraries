#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  ;; XXX
  (only (chezscheme) trace-define)
  (scheme-libraries testing)
  (scheme-libraries reading annotated-datums)
  (scheme-libraries syntax bootstrap-environment)
  (scheme-libraries syntax expressions)
  (scheme-libraries syntax expand)
  (scheme-libraries syntax syntax-objects))

(trace-define expand-datum
  (lambda (x)
    (expand (datum->annotated-datum x)
            (bootstrap-environment))))

(define-syntax test-expand
  (syntax-rules ()
    [(test-expand expected source)
     (test-assert (expression=? `expected (expand-datum `source)))]))

(test-begin "expand")

(test-expand (quote 10) 10)

(test-expand (lambda (x.0)
               x.0)
  (lambda (x) x))

(test-expand (lambda (x.0)
               (letrec* ([y.1 x.0])
                 y.1))
  (lambda (x)
    (define y x)
    y))

(test-expand (void)
  (void))

(test-expand (lambda ()
               (letrec* ([a.0 (void)])
                 '1))
  (lambda ()
    (define a)
    1))

(test-expand (if '#f '2 '3)
  (if #f 2 3))

(test-expand (if '#f '2 (values))
  (if #f 2))

(test-expand (quote #(1 2 3))
  '#(1 2 3))

(test-expand ((lambda (t.0)
                (if t.0 t.0 '2))
              '1)
  (or 1 2))

(test-expand ((lambda (t.0 t.1)
                '10)
              '2 '3)
  (let ([t.0 2] [t.1 3]) 10))

(test-expand (letrec ([x.0 x.0])
               x.0)
  (letrec ([x x]) x))

(test-expand (letrec* ([x.0 x.0])
               x.0)
  (letrec* ([x x]) x))

(test-expand (letrec ([f.0 (lambda (x.1) x.1)])
               (f.0 '0))
  (let f ([x 0])
    x))

(test-expand (begin '1 '2 '#f '3)
  (begin 1 (begin 2 #f) 3))

(test-expand (lambda () (letrec* ([x.0 '1] [y.1 '2]) x.0))
  (lambda () (begin (define x 1) (define y 2) x)))

(test-expand ((lambda (t.0) (if t.0 ('b t.0) 'c)) 'a)
  (cond ['a => 'b] [else 'c]))

(test-expand ((lambda (k.0) (if (memv k.0 '(3 4)) '#t (values))) '1)
  (case '1
    [(3 4) #t]
    [else #f]))

(test-expand (lambda () (letrec* ([x.0 '1]) (begin x.0 x.0)))
  (lambda () (begin (define x 1)) (begin x x)))

(test-expand (lambda () '#f)
  (lambda ()
    (define-syntax foo
      (lambda (stx)
        #f))
    foo))

(test-expand '2
  (letrec-syntax ([foo (lambda (x) #'bar)]
                  [bar (lambda (x) 2)])
    foo))

;;; Syntax-case

(test-expand '#t
  (let-syntax
      ([m (lambda (x)
            (syntax-case 3 ()
              [2 #f]
              [3 #t]))])
    m))

(test-expand '#t
  (let-syntax
      ([m (lambda (x)
            (syntax-case '(3 4) ()
              [2 #f]
              [(3 4) #t]))])
    m))

(test-expand '#t
  (let-syntax
      ([m (lambda (x)
            (syntax-case x ()
              [(_ 3) #f]
              [(_ 2) #t]))])
    (m 2)))

(test-expand '#t
  (let-syntax
      ([m (lambda (x)
            (syntax-case x (a b)
              [(_ b) #f]
              [(_ a) #t]))])
    (m a)))

(test-expand '#t
  (let-syntax
      ([m (lambda (x)
            (syntax-case x ()
              [(_ a) #'a]))])
    (m #t)))

(test-expand '#t
  (let-syntax
      ([m (lambda (x)
            (syntax-case x ()
              [(_ a) #f]
              [(_ a ...) #t]))])
    (m 1 2)))

(test-end "expand")
