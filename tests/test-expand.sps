#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries reading annotated-datums)
  (scheme-libraries syntax bootstrap-environment)
  (scheme-libraries syntax expressions)
  (scheme-libraries syntax expand)
  (scheme-libraries syntax syntax-objects))

(define expand-datum
  (lambda (x)
    (expand (datum->annotated-datum x)
            (bootstrap-environment))))

(define-syntax test-expand
  (syntax-rules ()
    [(test-expand expected source)
     (test-assert (expression=? `expected (expand-datum `source)))]))

(test-begin "expand")

(test-expand (quote 10) 10)

(test-expand (case-lambda [(x.0) x.0])
  (lambda (x) x))

(test-expand (case-lambda
               [(x.0)
                (letrec* ([y.1 x.0])
                  y.1)])
  (lambda (x)
    (define y x)
    y))

(test-expand (void)
  (void))

(test-expand (case-lambda
               [()
                (letrec* ([a.0 (void)])
                  '1)])
  (lambda ()
    (define a)
    1))

(test-expand (if '#f '2 '3)
  (if #f 2 3))

(test-expand (if '#f '2 (values))
  (if #f 2))

(test-expand (quote #(1 2 3))
  '#(1 2 3))

(test-expand ((case-lambda
                [(t.0)
                 (if t.0 t.0 '2)])
              '1)
  (or 1 2))

(test-expand ((case-lambda
                [(t.0 t.1)
                 '10])
              '2 '3)
  (let ([t.0 2] [t.1 3]) 10))

(test-expand (letrec ([x.0 x.0])
               x.0)
  (letrec ([x x]) x))

(test-expand (letrec* ([x.0 x.0])
               x.0)
  (letrec* ([x x]) x))

(test-expand (letrec ([f.0 (case-lambda [(x.1) x.1])])
               (f.0 '0))
  (let f ([x 0])
    x))

(test-expand (begin '1 '2 '#f '3)
  (begin 1 (begin 2 #f) 3))

(test-expand (case-lambda [() (letrec* ([x.0 '1] [y.1 '2]) x.0)])
  (lambda () (begin (define x 1) (define y 2) x)))

(test-expand ((case-lambda [(t.0) (if t.0 ('b t.0) 'c)]) 'a)
  (cond ['a => 'b] [else 'c]))

(test-expand ((case-lambda [(k.0) (if (memv k.0 '(3 4)) '#t '#f)]) '1)
  (case '1
    [(3 4) #t]
    [else #f]))

(test-expand (case-lambda [() (letrec* ([x.0 '1]) (begin x.0 x.0))])
  (lambda () (begin (define x 1)) (begin x x)))

;;; XXX: The following test fails.

(test-expand (case-lambda [() '#f])
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

(test-expand ('1 '2)
  (let-syntax
      ([m (lambda (x)
            (syntax-case x ()
              [(_ a) #f]
              [(_ a ...) #'(a ...)]))])
    (m 1 2)))

(test-expand ('1 '2)
  (let-syntax
      ([m (lambda (x)
            (syntax-case '#(1 2 3) ()
              [(a b c) #f]
              [#(1 2 4) #f]
              [#(a ... 3) #'(a ...)]))])
    m))

(test-expand '#(1 2 3)
  (let-syntax
      ([m (lambda (x)
            (syntax-case '(1 2 3) ()
              [(a ...) #''#(a ...)]))])
    m))

(test-expand ('2 '3)
  (let-syntax
      ([m (lambda (x)
            (with-syntax ([(_ a ...) x])
              #'(a ...)))])
    (m 2 3)))

(test-expand ('1 ('2 '3))
  (let-syntax
      ([m (lambda (x)
            #`(1 #,(list 2 3)))])
    m))

(test-expand ('2 '3)
  (let-syntax
      ([m (lambda (x)
            #`#,(list 2 3))])
    m))

(test-expand '#`(1 #,(list 2 3))
  (let-syntax
      ([m (lambda (x)
            #`'#`(1 #,(list 2 3)))])
    m))

(test-expand '((1 2) (3 4))
  (let-syntax
      ([m (lambda (x)
            #`'((unsyntax #'(1 2) #'(3 4))))])
    m))

(test-expand '(1 2 3 4)
  (let-syntax
      ([m (lambda (x)
            #`'((unsyntax-splicing #'(1 2) #'(3 4))))])
    m))

(test-expand '(1 2)
  (let-syntax
      ([m (lambda (x)
            #'`(1 2))])
    m))

(test-expand '`(1 2)
  (let-syntax
      ([m (lambda (x)
            #'``(1 2))])
    m))

(test-expand '`(1 ,2)
  (let-syntax
      ([m (lambda (x)
            #'``(1 ,2))])
    m))

(test-expand '2
  (let-syntax
      ([m (lambda (x)
            #'`,2)])
    m))

(test-expand (cons '1 (cons* '2 '()))
  (let-syntax
      ([m (lambda (x)
            #'`(1 ,2))])
    m))

(test-expand (list 'quasiquote (cons '1 (cons (cons 'unquote (cons* '2 '())) '())))
  (let-syntax
      ([m (lambda (x)
            #'``(1 ,,2))])
    m))

(test-expand (if '#t (begin '1 '2) (values))
  (when #t 1 2))

(test-expand (if '#t (values) (begin '1 '2))
  (unless #t 1 2))

(test-expand 'u
  (let-syntax
      ([m (syntax-rules ()
            [(k 1 2) 'k]
            [(k x) 'x])])
    (m u)))

(test-expand 'k
  (let-syntax
      ([m (syntax-rules ()
            [(k 1 2) 'k]
            [(k x) 'x])])
    (m 1 2)))

(test-expand '3
  (let-syntax
      ([m (identifier-syntax 3)])
    m))

(test-expand '4
  (let-syntax
      ([m (identifier-syntax
            [_ 4]
            [(set! _ e) #f])])
    m))

(test-expand 'b
  (let-syntax
      ([m (identifier-syntax
            [_ 4]
            [(set! _ e) e])])
    (set! m 'b)))

(test-end "expand")
