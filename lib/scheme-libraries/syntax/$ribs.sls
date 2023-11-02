#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $ribs)
  (export
    make-rib
    rib?
    rib-ref
    rib-for-each
    rib-map
    rib-set!
    rib-update!
    rib->s-exp
    s-exp->rib)
  (import
    (rnrs)
    ;; TODO: Avoid the use of mutable pairs.
    (rnrs mutable-pairs)
    (scheme-libraries define-who)
    (scheme-libraries match)
    (scheme-libraries syntax $marks))

  ;; Ribs

  (define make-rib
    (case-lambda
      [() (make-eq-hashtable)]
      [(k) (make-eq-hashtable k)]))

  (define rib?
    (lambda (x)
      (hashtable? x)))

  (define rib-ref
    (lambda (r n m)
      (cond
       [(hashtable-ref r n #f)
	=> (lambda (a)
	     (cond
              [(ass-marks m a) => cdr]
              [else #f]))]
       [else
	#f])))

  (define rib-for-each
    (lambda (f rib)
      (assert (procedure? f))
      (assert (rib? rib))
      (let-values ([(n* a*) (hashtable-entries rib)])
        (vector-for-each
         (lambda (n a)
           (for-each (lambda (p)
                       (f n (car p) (cdr p)))
             a))
         n* a*))))

  (define rib-map
    (lambda (f rib)
      (assert (procedure? f))
      (assert (rib? rib))
      (let-values ([(n* a*) (hashtable-entries rib)])
        (fold-left
          (lambda (x* n a)
            (fold-left
              (lambda (x* p)
                (cons (f n (car p) (cdr p))
                      x*))
              x* a))
          '()
          (vector->list n*)
          (vector->list a*)))))

  (define rib-set!
    (lambda (r n m l/p)
      (rib-update! r n m (lambda (x) l/p) #f)))

  (define rib-update!
    (lambda (r n m f d)
      (hashtable-update!
        r n
        (lambda (a)
          (cond
            [(ass-marks m a) =>
             (lambda (p)
               (set-cdr! p (f (cdr p)))
               a)]
            [else
              (cons (cons m (f d)) a)]))
        '())))

  ;; Serialization

  (define rib->s-exp
    (lambda (label/props->s-exp rib)
      (assert (procedure? label/props->s-exp))
      (assert (rib? rib))
      (rib-map
       (lambda (name marks l/p)
         `#(,name ,(mark-list->s-exp marks) ,(label/props->s-exp l/p)))
       rib)))

  (define s-exp->rib
    (lambda (s-exp->label/props e*)
      (let ([rib (make-rib (length e*))])
        (for-each
          (lambda (e)
            (match e
              [#(,n ,[s-exp->mark-list -> m] ,[s-exp->label/props -> l/p])
               (rib-set! rib n m l/p)]))
          e*)
        rib)))

  ;; Helpers

  (define ass-marks
    (lambda (marks a)
      (assp (lambda (m) (marks=? m marks)) a)))

  (define rem-marks
    (lambda (marks a)
      (remp (lambda (p) (marks=? (car p) marks)) a))))
