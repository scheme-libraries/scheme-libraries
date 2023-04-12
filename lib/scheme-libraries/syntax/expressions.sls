#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions)
  (export
    build
    expression?
    expression=?)
  (import
    (rnrs)
    (scheme-libraries atoms)
    (scheme-libraries define-who)
    (scheme-libraries match)
    (scheme-libraries syntax variables)
    (scheme-libraries with-implicit))

  (define-syntax/who build
    (lambda (x)
      (syntax-case x ()
        [(k tmpl)
         (with-implicit (k quasiquote)
           #'(extend-backquote k `tmpl))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define expression?
    (lambda (x)
      (let ([ht (make-eq-hashtable)])
        (let f ([x x])
          (cond
           [(atom? x)]
           [(hashtable-ref ht x #f) #f]
           [(pair? x)
            (hashtable-set! ht x #t)
            (and (f (car x)) (f (cdr x)))]
           [(vector? x)
            (hashtable-set! ht x #t)
            (let g ([k (vector-length x)])
              (or (fxzero? k)
                  (let ([k (fx- k 1)])
                    (and (f (vector-ref x k))
                         (g k)))))])))))

  (define/who expression=?
    (lambda (x y)
      (unless (expression? x)
        (assertion-violation who "invalid expression argument" x))
      (unless (expression? y)
        (assertion-violation who "invalid expression argument" y))
      (let ([ht (make-eq-hashtable)])
        (let f ([x x] [y y])
          (cond
           [(variable? x)
            (and (variable? y)
                 (cond
                  [(hashtable-ref ht x #f)
                   => (lambda (x)
                        (eq? x y))]
                  [else
                   (hashtable-set! ht x y)
                   #t]))]
           [(pair? x)
            (and (pair? y)
                 (f (car x) (car y))
                 (f (cdr x) (cdr y)))]
           [(vector? x)
            (and (vector? y)
                 (let ([n (vector-length x)])
                   (and (fx=? n (vector-length y))
                        (let g ([k n])
                          (or (fxzero? k)
                              (let ([k (fx- k 1)])
                                (and (f (vector-ref x k)
                                        (vector-ref y k))
                                     (g k))))))))]
           [else (atom=? x y)])))))
  )