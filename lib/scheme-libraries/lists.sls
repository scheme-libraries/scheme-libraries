#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries lists)
  (export
    length+
    split-at)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries exceptions)
    (scheme-libraries numbers))

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
          (index-out-of-range-violation k)])))))
