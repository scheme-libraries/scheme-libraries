#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (only (chezscheme) display-statistics)
  (scheme-libraries testing)
  (scheme-libraries syntax bootstrap-environment)
  (scheme-libraries syntax library-collections)
  (scheme-libraries syntax default-stdlibs-collections)
  (scheme-libraries syntax eval)
  (scheme-libraries syntax import-specs)
  (scheme-libraries syntax library-loaders)
  (scheme-libraries syntax library-locators)
  (scheme-libraries syntax syntax-objects))

(current-library-collection (make-default-stdlibs-collection))

(define library-locator (make-library-locator '("tests/lib/") '(".sls")))
(current-library-loader (make-default-library-loader library-locator))

(define test-environment
  (environment '(rnrs)
               '(rnrs eval)
               '(scheme-libraries define-who)))

(define-syntax test-eval
  (syntax-rules ()
    [(test-eval result expr)
     (begin
       ;; FIXME/DEBUG:
       ;;(eval 'expr test-environment)
       (test-equal result (eval 'expr test-environment)))]))

(test-begin "eval")

(test-equal 4 (eval '4 (bootstrap-environment)))

(test-equal '(1 2 3) (eval '`(1 2 ,3) (bootstrap-environment)))
(test-equal '(1 2 3) (eval '`(1 2 ,'3) (bootstrap-environment)))

(test-equal 1 (eval 1 (environment '(rnrs base))))

(test-equal 12 (eval '(foo) (environment '(test))))

(test-equal 10
  (eval '(let ([x 10]) x) (environment '(rnrs base))))

(test-equal 13
  (eval '(let*-values ([(x) 13]) x) (environment '(rnrs base))))

(test-equal 14
  (eval '(let*-values () 14) (environment '(rnrs base))))

(test-equal '(1 2 3)
  (eval '(let*-values ([(x y z) (values 1 2 3)]) (list x y z)) (environment '(rnrs base))))

(test-equal '(1 (2 3) 5)
  (eval '(let ([x 5])
           (let-values ([(x . y) (values 1 2 3)]
                        [(z) x])
             (list x y z)))
        (environment '(rnrs base))))

(test-assert (eval 'list (environment '(rnrs base))))

;;; Identifier properties

(test-assert (eval '(let ([x 1]
                          [y 2])
                      (define-property x y #f)
                      #t)
                   (bootstrap-environment)))

(test-assert (eval '(let ([x 1]
                          [y 2])
                      (define-syntax m
                        (lambda (x)
                          (lambda (lookup)
                            (syntax-case x ()
                              [(_) #'#t]))))
                      (define-property x y #f)
                      (m))
                   (bootstrap-environment)))

(test-assert (eval '(let ([x 1]
                          [y 2])
                      (define-syntax m
                        (lambda (stx)
                          (lambda (lookup)
                            (syntax-case stx ()
                              [(_) (lookup #'x #'y)]))))
                      (define-property x y #t)
                      (m))
                   (bootstrap-environment)))

(test-eval 1 1)

(test-eval #t
  (let ()
    (define-record-type pare
      (fields kar kdr))
    (pare? (make-pare 1 2))))

(test-eval '(1 3)
  (let ()
    (define-record-type pare
      (fields kar (mutable kdr kdr set-kdr!)))
    (let ([x (make-pare 1 2)])
      (set-kdr! x 3)
      (list (pare-kar x) (kdr x)))))

(test-eval '(1 2)
  (let ()
    (define-record-type base)
    (define-record-type pare
      (fields kar (mutable kdr kdr set-kdr!))
      (parent base))
    (let ([x (make-pare 1 2)])
      (list (pare-kar x) (kdr x)))))

(test-eval '(1 2)
  (let ()
    (define-record-type base)
    (define-record-type pare
      (fields kar (mutable kdr kdr set-kdr!))
      (parent-rtd (record-type-descriptor base) #f))
    (let ([x (make-pare 1 2)])
      (list (pare-kar x) (kdr x)))))

(test-eval #t
    (let ()
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
      #t))

(test-eval 3
  (let ()
    (define-record-type pattern-variable
      (nongenerative) (sealed #t) (opaque #t)
      (fields (mutable identifier) expression level))
    (pattern-variable-level (make-pattern-variable #f #f 3))))

(test-eval '(1 1 1)
  (let ()
    (define/who make-list
      (case-lambda
        [(k fill)
         (do [(k k (fx- k 1))
              (rv '() (cons fill rv))]
             ((fxzero? k)
              rv))]
        [(k) (make-list k #f)]))
    (make-list 3 1)))

(test-eval 3
  (let ([y 3])
    y))

(test-eval 5
  (let* ([x 12]
         [x (- x 7)])
    x))

(test-assert
  (condition?
   (eval '(make-implementation-restriction-violation) test-environment)))

(test-assert
  (condition?
   (eval '(let ()
            (define-condition-type &foo &condition
              make-foo-condition foo-condition?)
            (make-foo-condition))
         test-environment)))

(test-eval 4
  (eval '4 (environment '(rnrs))))

(test-eval 5
  (eval '(+ 1 4) (environment '(rnrs))))

(test-eval 'apple
  (eval '(eval '(car '(apple pear)) (environment '(rnrs)))
        (environment '(rnrs) '(rnrs eval))))

(test-end "eval")

(display-statistics)

;; Local Variables:
;; eval: (put 'test-eval 'scheme-indent-function 1)
;; End:
