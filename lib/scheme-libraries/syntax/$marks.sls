#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $marks)
  (export
    make-mark
    mark-name
    mark?
    mark-list?
    mark=?
    marks=?
    anti-mark
    anti-mark?
    mark->symbol
    symbol->mark
    mark-list->s-exp
    s-exp->mark-list)
  (import
    (rnrs)
    (scheme-libraries hashtables)
    (scheme-libraries define-values)
    (scheme-libraries define-who)
    (scheme-libraries record-writer)
    (scheme-libraries uuid))

  ;; Marks

  (define-record-type mark
    (nongenerative mark-7f4e42be-fbaf-44b9-a5ee-64fd0190fed3)
    (sealed #t)
    (fields (mutable name))
    (protocol
      (lambda (new)
        (define who 'make-mark)
        (case-lambda
          [() (new #f)]
          [(name)
           (unless (symbol? name)
             (assertion-violation who "not a valid name argument" name))
           (new name)]))))

  (define mark-list?
    (lambda (m)
      (and (list? m)
           (for-all mark? m))))

  (define/who mark=?
    (lambda (m1 m2)
      (unless (mark? m1)
        (assertion-violation who "invalid first mark argument" m1))
      (unless (mark? m2)
        (assertion-violation who "invalid second mark argument" m2))
      (or (eq? m1 m2)
          (let ([name1 (mark-name m1)]
                [name2 (mark-name m2)])
            (and name1 name2 (symbol=? name1 name2))))))

  (define/who marks=?
    (lambda (m1* m2*)
      (unless (mark-list? m1*)
        (assertion-violation who "invalid first mark list argument" m1*))
      (unless (mark-list? m2*)
        (assertion-violation who "invalid second mark list argument" m2*))
      (if (null? m1*) (null? m2*)
          (and (pair? m2*)
               (eq? (car m1*) (car m2*))
               (marks=? (cdr m1*) (cdr m2*))))))

  (define/who member-mark
    (lambda (m m*)
      (unless (mark? m)
        (assertion-violation who "invalid mark argument" m))
      (unless (mark-list? m*)
        (assertion-violation who "invalid mark list argument" m*))
      (exists (lambda (x) (mark=? m x)) m*)))

  (define anti-mark
    (let ([mark (make-mark 'anti)])
      (lambda () mark)))

  (define/who anti-mark?
    (lambda (m)
      (unless (mark? m)
        (assertion-violation who "invalid mark argument" m))
      (mark=? m (anti-mark))))

  ;; Serializing

  (define/who mark->symbol
    (lambda (m)
      (unless (mark? m)
        (assertion-violation who "invalid mark argument" m))
      (or (mark-name m)
          (let ([name (uid 'mark)])
            (mark-name-set! m name)
            name))))

  (define/who symbol->mark
    (lambda (sym)
      (unless (symbol? sym)
        (assertion-violation who "invalid symbol argument" sym))
      (make-mark sym)))

  (define mark-list->s-exp
    (lambda (m)
      (map mark->symbol m)))

  (define s-exp->mark-list
    (lambda (e)
      (assert (list? e))
      (map symbol->mark e)))

  ;; Record writers

  (record-writer (record-type-descriptor mark)
    (lambda (r p wr)
      (put-string p "#<mark ")
      (put-string p (symbol->string (mark->symbol r)))
      (put-string p ">"))))
