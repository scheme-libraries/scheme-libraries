#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries lists)
  (export
    last
    iota
    filter-map
    make-list
    length+
    split-at
    take
    delete-duplicates)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries exceptions)
    (scheme-libraries numbers)
    (scheme-libraries void))

  (define/who last
    (lambda (ls)
      (unless (pair? ls)
        (assertion-violation who "invalid list argument" ls))
      (car (reverse ls))))

  (define/who iota
    (case-lambda
      [(count start step)
       (unless (nonnegative-fixnum? count)
         (assertion-violation who "invalid count argument" count))
       (unless (number? start)
         (assertion-violation who "invalid start argument" start))
       (unless (number? step)
         (assertion-violation who "invalid step argument" start))
       (let f ([count count]
               [start start])
         (if (fxzero? count)
             '()
             (cons start (f (fx- count 1)
                            (+ start step)))))]
      [(count) (iota count 0 1)]))

  (define/who filter-map
    (lambda (proc . list*)
      (unless (procedure? proc)
        (assertion-violation who "invalid procedure argument" proc))
      (for-each
       (lambda (list)
         (unless (list? list)
           (assertion-violation who "invalid list argument" list)))
       list*)
      (reverse
       (apply fold-left
              (lambda (acc . el*)
                (cond
                 [(apply proc el*)
                  => (lambda (el)
                       (cons el acc))]
                 [else acc]))
              '() list*))))

  (define length+
    (lambda (x)
      (let f ([x x] [y x] [n 0])
        (if (pair? x)
            (let ([x (cdr x)]
                  [n (fx+ n 1)])
              (if (pair? x)
                  (let ([x (cdr x)]
                        [y (cdr y)]
                        [n (fx+ n 1)])
                    (and (not (eq? x y))
                         (f x y n)))
                  n))
            n))))

  (define/who split-at
    (lambda (ls k)
      (define index-out-of-range-violation
        (lambda (k)
          (assertion-violationf who "index ~s out of range for list ~a" k ls)))
      (unless (nonnegative-fixnum? k)
        (assertion-violation who "invalid index argument" k))
      (let f ([ls ls] [k k])
        (cond
         [(fxzero? k)
          (values '() ls)]
         [(pair? ls)
          (let-values ([(ls1 ls2) (f (cdr ls) (fx- k 1))])
            (values (cons (car ls) ls1) ls2))]
         [else
          (index-out-of-range-violation k)]))))

  (define/who make-list
    (case-lambda
      [(k fill)
       (unless (nonnegative-fixnum? k)
         (assertion-violation who "invalid length argument" k))
       (do [(k k (fx- k 1))
            (rv '() (cons fill rv))]
           ((fxzero? k)
            rv))]
      [(k) (make-list k (void))]))

  (define/who take
    (lambda (x i)
      (unless (nonnegative-fixnum? i)
        (assertion-violation who "invalid length argument" i))
      (let f ([x x] [i i])
        (cond
         [(fxzero? i) '()]
         [(null? x)
          (assertion-violation who "index out of range" x i)]
         [else
          (cons (car x) (f (cdr x) (fx- i 1)))]))))

  (define/who delete-duplicates
    (lambda (ls =?)
      (unless (list? ls)
        (assertion-violation who "invalid list argument" ls))
      (unless (procedure? =?)
        (assertion-violation who "invalid equivalence predicate argument" =?))
      (let f ([ls ls] [rev-ls '()])
	(if (null? ls)
	    (reverse rev-ls)
	    (let ([y (car ls)]
		  [ls (cdr ls)])
	      (if (exists
		   (lambda (x)
		     (=? x y))
		   rev-ls)
		  (f ls rev-ls)
		  (f ls (cons y rev-ls)))))))))
