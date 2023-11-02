#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $ribcages)
  (export
    make-extensible-ribcage
    make-fixed-ribcage
    ribcage?
    ribcage-empty?
    ribcage-ref
    ribcage-set!
    ribcage-add-barrier!
    &duplicate-definition
    make-duplicate-definition-condition
    duplicate-definition-condition?
    duplicate-definition-name
    ribcage->s-exp
    s-exp->ribcage)
  (import
    (rnrs)
    (scheme-libraries numbers)
    (scheme-libraries match)
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
      (unless (symbol? n)
        (assertion-violation who "invalid name argument" n))
      (unless (mark-list? m)
        (assertion-violation who "invalid mark list argument" m))
      (unless (procedure? fail)
        (assertion-violation who "invalid failure thunk argument" fail))
      (cond
       [(fixed-ribcage? r)
        (let ([names (fixed-ribcage-names r)]
              [mark-lists (fixed-ribcage-mark-lists r)]
              [label/props (fixed-ribcage-label/props r)])
          (let ([k (vector-length names)])
            (let f ([i 0])
              (cond
               [(fx=? i k)
                (fail)]
               [(and (eq? (vector-ref names i) n)
                     (marks=? (vector-ref mark-lists i) m))
                (vector-ref label/props i)]
               [else (f (fx+ i 1))]))))]
       [(extensible-ribcage? r)
        (let f ([chunks (extensible-ribcage-chunks r)])
	  (let-values ([(table barrier chunks)
		        (next-chunk chunks)])
	    (or (rib-ref table n m)
	        (if barrier
		    (and (not (barrier-blocks? barrier m))
		         (f chunks))
		    (fail)))))]
       [else (assertion-violation who "invalid ribcage argument" r)])))

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
      (let-values ([(table barrier chunks) (next-chunk (extensible-ribcage-chunks r))])
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
      (unless (extensible-ribcage? r)
        (assertion-violation who "invalid ribcage argument" r))
      (unless (rib? rib)
        (assertion-violation who "invalid rib argument" rib))
      (unless (and (list? mark-list*)
		   (for-all mark-list? mark-list*))
        (assertion-violation who "invalid list of mark lists argument" mark-list*))
      (extensible-ribcage-chunks-set!
       r
       (cons* rib
	      (delete-duplicates mark-list* marks=?)
	      (extensible-ribcage-chunks r)))))

  (define/who ribcage-for-each
    (lambda (proc r)
      (unless (procedure? proc)
        (assertion-violation who "invalid procedure argument" proc))
      (unless (ribcage? r)
        (assertion-violation who "invalid ribcage argument" r))
      (rib-for-each proc (car (extensible-ribcage-chunks r)))))

  ;; Ribcages

  (define-record-type ribcage
    (nongenerative ribcage-f0addb1f-f1bd-4a09-af5d-91459db5979b))

  (define ribcage-empty?
    (lambda (r)
      (and (fixed-ribcage? r)
           (fxzero? (vector-length (fixed-ribcage-names r))))))

  ;; Extensible ribcages

  (define-record-type extensible-ribcage
    (nongenerative extensible-ribcage-4f79e972-eeda-436e-aa68-fff3f6ee27b2)
    (parent ribcage)
    (sealed #t)
    (fields (mutable chunks))
    (protocol
      (lambda (pargs->new)
        (define who 'make-extensible-ribcage)
        (rec make
          (case-lambda
            [() ((pargs->new) (list (make-rib)))]
            [(rib) ((pargs->new) (list rib))]
            #;
            [(n* m* lbl*)               ;
            (unless (and (list? n*)     ;
            (for-all symbol? n*))       ;
            (assertion-violation who "invalid name list argument" n*)) ;
            (unless (and (list? m*)     ;
            (for-all mark-list? m*))    ;
            (assertion-violation who "invalid list of mark lists argument" m*)) ;
            (unless (and (list? lbl*)   ;
            (for-all label? lbl*))      ;
            (assertion-violation who "invalid label list argument" lbl*)) ;
            (let ([rib (make-rib)])     ;
            (for-each                   ;
            (lambda (n m lbl)           ;
            (rib-set! rib n m (make-label/props lbl))) ;
            n* m* lbl*)                 ;
            ((pargs->new) (list rib)))])))))

  (define chunks->ribcage
    (record-constructor
     (make-record-constructor-descriptor (record-type-descriptor extensible-ribcage) #f #f)))

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

  (define-record-type fixed-ribcage
    (nongenerative fixed-ribcage-9ca4ac9e-8388-421f-ac14-b6db4bdd02f6)
    (parent ribcage)
    (sealed #t)
    (fields names mark-lists label/props)
    (protocol
      (lambda (pargs->new)
        (lambda (n* m* lbl*)
          ((pargs->new)
           (list->vector n*)
           (list->vector m*)
           (vector-map (lambda (lbl)
                         (make-label/props lbl))
                       (list->vector lbl*)))))))

  (define vectors->ribcage
    (record-constructor
     (make-record-constructor-descriptor (record-type-descriptor fixed-ribcage) #f #f)))

  ;; Serialization

  (define ribcage->s-exp
    (lambda (label/props->s-exp r)
      (cond
       [(fixed-ribcage? r)
        `(fixed
          ,(fixed-ribcage-names r)
          ,(fixed-ribcage-mark-lists r) ;We assume that marks are serializable.
          ,(vector-map label/props->s-exp (fixed-ribcage-label/props r)))]
       [(extensible-ribcage? r)
        `(extensible
          ,@(let f ([chunks (extensible-ribcage-chunks r)])
              (let-values ([(table barrier chunks)
                            (next-chunk chunks)])
                (if chunks
                    (cons* (rib->s-exp label/props->s-exp table)
                           (barrier->s-exp barrier)
                           (f chunks))
                    (list (rib->s-exp label/props->s-exp table))))))]
       [else (assert #f)])))

  (define barrier->s-exp
    (lambda (mark-list*)
      (map mark-list->s-exp mark-list*)))

  (define s-exp->ribcage
    (lambda (s-exp->label/props e)
      (match e
        [(fixed ,names ,mark-lists ,label/props)
         (vectors->ribcage names
                           mark-lists
                           (vector-map s-exp->label/props label/props))]
        [(extensible ,e* ...)
         (chunks->ribcage
          (let f ([e* e*])
            (let-values ([(e b e*) (next-chunk e*)])
              (if e*
                  (cons* (s-exp->rib s-exp->label/props e)
                         (s-exp->barrier b)
                         (f e*))
                  (list (s-exp->rib s-exp->label/props e))))))])))

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
