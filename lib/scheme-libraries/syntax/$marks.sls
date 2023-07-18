#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $marks)
  (export
    make-mark
    mark?
    mark-list?
    mark=?
    marks=?
    anti-mark
    anti-mark?
    mark->datum
    datum->mark)
  (import
    (rnrs)
    (scheme-libraries gensyms)
    (scheme-libraries hashtables)
    (scheme-libraries define-values)
    (scheme-libraries define-who)
    (scheme-libraries record-writer))

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
      (eq? m1 m2)))

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

  ;; Serializers

  (define/who mark->datum
    (let ([ht (make-eq-hashtable)])
      (hashtable-set! ht (anti-mark) 'anti)
      (lambda (m)
        (assert (mark? m))
        (intern! ht
                 m
                 (lambda ()
                   (gensym "m"))))))

  (define/who datum->mark
    (let ([ht (make-eq-hashtable)])
      (hashtable-set! ht 'anti (anti-mark))
      (lambda (s)
        (assert (symbol? s))
        (intern! ht
                 s
                 (lambda ()
                   (make-mark))))))

  ;; Record writers

  (record-writer (record-type-descriptor mark)
    (lambda (r p wr)
      (define name (mark-name r))
      (put-string p "#<mark ")
      (cond
       [(symbol? name)
        (put-string p (symbol->string name))]
       [(number? name)
        (put-string p (number->string name))]
       [(not name)
        (let ([name (mark->datum r)])
          (mark-name-set! r name)
          (put-string p (number->string name)))]
       [else (assert #f)])
      (put-string p ">"))))
