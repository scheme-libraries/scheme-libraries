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
     (begin
       (test-assert (expression=? `expected (expand-datum `source))))]))

(test-begin "expand")

(expand-datum
 '(lambda ()
    (define-syntax foo
      (lambda (stx)
        #f))
    foo))

;; (test-expand '2
;;   (letrec-syntax ([foo (lambda (x) #'bar)]
;;                   [bar (lambda (x) 2)])
;;     foo))

;; ;;; Syntax-case

;; (test-expand '#t
;;   (let-syntax
;;       ([m (lambda (x)
;;             (syntax-case 3 ()
;;               [2 #f]
;;               [3 #t]))])
;;     m))

;; (test-expand '#t
;;   (let-syntax
;;       ([m (lambda (x)
;;             (syntax-case '(3 4) ()
;;               [2 #f]
;;               [(3 4) #t]))])
;;     m))

;; (test-expand '#t
;;   (let-syntax
;;       ([m (lambda (x)
;;             (syntax-case x ()
;;               [(_ 3) #f]
;;               [(_ 2) #t]))])
;;     (m 2)))

;; (test-expand '#t
;;   (let-syntax
;;       ([m (lambda (x)
;;             (syntax-case x (a b)
;;               [(_ b) #f]
;;               [(_ a) #t]))])
;;     (m a)))

;; (test-expand '#t
;;   (let-syntax
;;       ([m (lambda (x)
;;             (syntax-case x ()
;;               [(_ a) #'a]))])
;;     (m #t)))

;; (test-expand '#t
;;   (let-syntax
;;       ([m (lambda (x)
;;             (syntax-case x ()
;;               [(_ a) #f]
;;               [(_ a ...) #t]))])
;;     (m 1 2)))

;; (test-expand ('1 '2)
;;   (let-syntax
;;       ([m (lambda (x)
;;             (syntax-case x ()
;;               [(_ a) #f]
;;               [(_ a ...) #'(a ...)]))])
;;     (m 1 2)))

;; (test-expand ('1 '2)
;;   (let-syntax
;;       ([m (lambda (x)
;;             (syntax-case '#(1 2 3) ()
;;               [(a b c) #f]
;;               [#(1 2 4) #f]
;;               [#(a ... 3) #'(a ...)]))])
;;     m))

;; (test-expand '#(1 2 3)
;;   (let-syntax
;;       ([m (lambda (x)
;;             (syntax-case '(1 2 3) ()
;;               [(a ...) #''#(a ...)]))])
;;     m))

;; (test-expand ('2 '3)
;;   (let-syntax
;;       ([m (lambda (x)
;;             (with-syntax ([(_ a ...) x])
;;               #'(a ...)))])
;;     (m 2 3)))

;; (test-expand ('1 ('2 '3))
;;   (let-syntax
;;       ([m (lambda (x)
;;             #`(1 #,(list 2 3)))])
;;     m))

;; (test-expand ('2 '3)
;;   (let-syntax
;;       ([m (lambda (x)
;;             #`#,(list 2 3))])
;;     m))

;; (test-expand '#`(1 #,(list 2 3))
;;   (let-syntax
;;       ([m (lambda (x)
;;             #`'#`(1 #,(list 2 3)))])
;;     m))

;; (test-expand '((1 2) (3 4))
;;   (let-syntax
;;       ([m (lambda (x)
;;             #`'((unsyntax #'(1 2) #'(3 4))))])
;;     m))

;; (test-expand '(1 2 3 4)
;;   (let-syntax
;;       ([m (lambda (x)
;;             #`'((unsyntax-splicing #'(1 2) #'(3 4))))])
;;     m))

;; (test-expand '(1 2)
;;   (let-syntax
;;       ([m (lambda (x)
;;             #'`(1 2))])
;;     m))

;; (test-expand '`(1 2)
;;   (let-syntax
;;       ([m (lambda (x)
;;             #'``(1 2))])
;;     m))

;; (test-expand '`(1 ,2)
;;   (let-syntax
;;       ([m (lambda (x)
;;             #'``(1 ,2))])
;;     m))

;; (test-expand '2
;;   (let-syntax
;;       ([m (lambda (x)
;;             #'`,2)])
;;     m))

;; (test-expand (cons '1 (cons* '2 '()))
;;   (let-syntax
;;       ([m (lambda (x)
;;             #'`(1 ,2))])
;;     m))

;; (test-expand (list 'quasiquote (cons '1 (cons (cons 'unquote (cons* '2 '())) '())))
;;   (let-syntax
;;       ([m (lambda (x)
;;             #'``(1 ,,2))])
;;     m))

;; (test-expand (if '#t (begin '1 '2) (values))
;;   (when #t 1 2))

;; (test-expand (if '#t (values) (begin '1 '2))
;;   (unless #t 1 2))

;; (test-expand 'u
;;   (let-syntax
;;       ([m (syntax-rules ()
;;             [(k 1 2) 'k]
;;             [(k x) 'x])])
;;     (m u)))

;; (test-expand 'k
;;   (let-syntax
;;       ([m (syntax-rules ()
;;             [(k 1 2) 'k]
;;             [(k x) 'x])])
;;     (m 1 2)))

;; (test-expand '3
;;   (let-syntax
;;       ([m (identifier-syntax 3)])
;;     m))

;; (test-expand '4
;;   (let-syntax
;;       ([m (identifier-syntax
;;             [_ 4]
;;             [(set! _ e) #f])])
;;     m))

;; (test-expand 'b
;;   (let-syntax
;;       ([m (identifier-syntax
;;             [_ 4]
;;             [(set! _ e) e])])
;;     (set! m 'b)))

(test-end "expand")
