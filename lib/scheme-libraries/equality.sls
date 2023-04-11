#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries equality)
  (export
    make-equal?)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries union-find))

  ;; TODO: Implement the fast version of Michael D. Adams and R. Kent
  ;; Dybvig: Efficient Nondestructive Equality Checking for Trees and
  ;; Graphs.

  (define/who make-equal?
    (lambda (equiv?)
      (unless (procedure? equiv?)
        (assertion-violation who "invalid equivalence predicate argument" equiv?))
      (lambda (x y)
        (let ([ht (make-eq-hashtable)])
          (let e? ([x x] [y y])
            (or (eq? x y)
                (cond
                 [(pair? x)
                  (and (pair? y)
                       (or (union-find ht x y)
                           (and (e? (car x) (car y))
                                (e? (cdr x) (cdr y)))))]
                 [(vector? x)
                  (and (vector? y)
                       (let ([n (vector-length x)])
                         (and (fx=? (vector-length y) n)
                              (or (union-find ht x y)
                                  (let f ([i 0])
                                    (or (fx=? i n)
                                        (and (e? (vector-ref x i)
                                                 (vector-ref y i))
                                             (f (fx+ i 1)))))))))]
                 [else
                  (equiv? x y)]))))))

    )
  )
