#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $ribcages)
  (export
    make-ribcage
    ribcage?
    ribcage-ref
    ribcage-set!
    ribcage-add-barrier!
    &duplicate-definition
    make-duplicate-definition-condition
    duplicate-definition-condition?
    duplicate-definition-name
    ribcage->s-exp
    s-exp->ribcage
    )
  (import
    (rnrs)
    (scheme-libraries numbers)
    (scheme-libraries define-who)
    (scheme-libraries lists)
    (scheme-libraries parameters)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax $marks)
    (scheme-libraries syntax $ribs)
    (scheme-libraries rec)
    (scheme-libraries record-writer))

  (define/who ribcage-ref
    (lambda (r n m fail)
      (unless (ribcage? r)
        (assertion-violation who "invalid ribcage argument" r))
      (unless (symbol? n)
        (assertion-violation who "invalid name argument" n))
      (unless (mark-list? m)
        (assertion-violation who "invalid mark list argument" m))
      (unless (procedure? fail)
        (assertion-violation who "invalid failure thunk argument" fail))
      (let f ([chunks (ribcage-chunks r)])
	(let-values ([(table barrier chunks)
		      (next-chunk chunks)])
	  (or (rib-ref table n m)
	      (if barrier
		  (and (not (barrier-blocks? barrier m))
		       (f chunks))
		  (fail)))))))

  (define/who ribcage-set!
    (lambda (r n m l/p)
      (unless (ribcage? r)
        (assertion-violation who "invalid ribcage argument" r))
      (unless (symbol? n)
        (assertion-violation who "invalid name argument" n))
      (unless (mark-list? m)
        (assertion-violation who "invalid mark list argument" m))
      (unless (label/props? l/p)
        (assertion-violation who "invalid label/props argument" l/p))
      (let-values ([(table barrier chunks) (next-chunk (ribcage-chunks r))])
	(define fail
	  (lambda ()
	    (rib-set! table n m l/p)))
	(define succeed
	  (lambda (prev-l/p)
	    (unless (label=? (label/props-label l/p)
			     (label/props-label prev-l/p))
	      (raise (make-duplicate-definition-condition n)))
	    (rib-set! table n m (label/props-merge l/p prev-l/p))))
	;; XXX: Can we simplify the logic below?
	(cond
	 [(rib-ref table n m) => succeed]
	 [else
	  (let f ([barrier barrier] [chunks chunks])
	    (if (or (not barrier)
                    (barrier-blocks? barrier m))
		(fail)
		(let-values ([(table barrier chunks)
			      (next-chunk chunks)])
		  [cond
		   [(rib-ref table n m) => succeed]
		   [else
		    (f barrier chunks)]])))]))))

  (define/who ribcage-add-barrier!
    (lambda (r rib mark-list*)
      (unless (ribcage? r)
        (assertion-violation who "invalid ribcage argument" r))
      (unless (rib? rib)
        (assertion-violation who "invalid rib argument" rib))
      (unless (and (list? mark-list*)
		   (for-all mark-list? mark-list*))
        (assertion-violation who "invalid list of mark lists argument" mark-list*))
      (ribcage-chunks-set!
       r
       (cons* rib
	      (delete-duplicates mark-list* marks=?)
	      (ribcage-chunks r)))))

  (define/who ribcage-for-each
    (lambda (proc r)
      (unless (procedure? proc)
        (assertion-violation who "invalid procedure argument" proc))
      (unless (ribcage? r)
        (assertion-violation who "invalid ribcage argument" r))
      (rib-for-each proc (car (ribcage-chunks r)))))

  ;; Ribcages

  (define-record-type ribcage
    (nongenerative ribcage-4f79e972-eeda-436e-aa68-fff3f6ee27b2)
    (sealed #t)
    (fields (mutable chunks))
    (protocol
      (lambda (new)
        (define who 'make-ribcage)
        (rec make
          (case-lambda
            [() (new (list (make-rib)))]
            [(rib) (new (list rib))]
            [(n* m* lbl*)
             (unless (and (list? n*)
                          (for-all symbol? n*))
               (assertion-violation who "invalid name list argument" n*))
             (unless (and (list? m*)
                          (for-all mark-list? m*))
               (assertion-violation who "invalid list of mark lists argument" m*))
             (unless (and (list? lbl*)
                          (for-all label? lbl*))
               (assertion-violation who "invalid label list argument" lbl*))
             (let ([rib (make-rib)])
               (for-each
                (lambda (n m lbl)
                  (rib-set! rib n m (make-label/props lbl)))
                n* m* lbl*)
               (new (list rib)))])))))

  (define chunks->ribcage
    (record-constructor
     (make-record-constructor-descriptor (record-type-descriptor ribcage) #f #f)))

  (define next-chunk
    (lambda (chunks)
      (let ([table (car chunks)]
	    [chunks (cdr chunks)])
	(if (null? chunks)
	    (values table #f #f)
	    (values table (car chunks) (cdr chunks))))))

  (define barrier-blocks?
    (lambda (barrier m)
      (assert (list? barrier))
      (exists (lambda (barrier-marks)
		(marks=? barrier-marks m))
	      barrier)))

  ;; Fixed rib cages

  (define make-fixed-ribcage
    (lambda (n* m* lbl*)
      (vector n* m* lbl*)))

  (define fixed-ribcage?
    (lambda (obj)
      (vector? obj)))

  ;; Serialization

  (define ribcage->s-exp
    (lambda (label/props->s-exp r)
      (let f ([chunks (ribcage-chunks r)])
        (let-values ([(table barrier chunks)
                      (next-chunk chunks)])
          (if chunks
              (cons* (rib->s-exp label/props->s-exp table)
                     (barrier->s-exp barrier)
                     (f chunks))
              (list (rib->s-exp label/props->s-exp table)))))))

  (define barrier->s-exp
    (lambda (mark-list*)
      (map mark-list->s-exp mark-list*)))

  (define s-exp->ribcage
    (lambda (s-exp->label/props e*)
      (define chunks
        (let f ([e* e*])
          (let-values ([(e b e*) (next-chunk e*)])
            (if e*
                (cons* (s-exp->rib s-exp->label/props e)
                       (s-exp->barrier b)
                       (f e*))
                (list (s-exp->rib s-exp->label/props e))))))
      (chunks->ribcage chunks)))

  (define s-exp->barrier
    (lambda (e*)
      (map s-exp->mark-list e*)))

  ;; Conditions

  (define-condition-type &duplicate-definition
    &condition
    make-duplicate-definition-condition duplicate-definition-condition?
    (name duplicate-definition-name))

  ;; Record writers

  (record-writer (record-type-descriptor ribcage)
    (lambda (r p wr)
      (put-string p "#<ribcage>"))))
