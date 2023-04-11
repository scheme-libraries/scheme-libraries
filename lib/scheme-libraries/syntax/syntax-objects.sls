#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

  (define-condition-type &syntax-error &error
    make-syntax-error syntax-error?
    (form syntax-error-form)
    (subform syntax-error-subform))


(library (scheme-libraries syntax syntax-objects)
  (export
    metalevel?
    metalevel:syntax
    metalevel:run
    core-environment
    environment?
    syntax-object?
    annotated-datum->syntax-object
    syntax-object-source-location
    syntax-object->datum
    datum->syntax-object
    syntax-atom?
    syntax-null?
    syntax-pair?
    syntax-car
    syntax-cdr
    syntax-vector?
    syntax-vector->list
    $identifier?
    $bound-identifier=?
    $free-identifier=?
    (rename &syntax $&syntax)
    make-syntax-error
    syntax-error?
    syntax-error-form
    syntax-error-subform)
  (import
    (except (rnrs) &syntax)
    (rnrs mutable-pairs)
    (scheme-libraries atoms)
    (scheme-libraries counters)
    (scheme-libraries define-values)
    (scheme-libraries define-who)
    (scheme-libraries lists)
    (scheme-libraries numbers)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries reading source-locations)
    (scheme-libraries rec)
    (scheme-libraries record-writer))

  ;; Bindings

  (define-record-type binding
    (nongenerative binding-749b4948-3923-484f-b466-3e8a9048a931))

  (define-record-type displaced-binding
    (nongenerative displaced-binding-ac5ee8c9-fc00-4e4d-919e-90373972d283)
    (parent binding) (sealed #t))

  (define-record-type out-of-phase-binding
    (nongenerative out-of-phase-binding-9f0102df-6804-4d78-b919-27057da676bc)
    (parent binding)
    (sealed #t))

  ;; Metalevels

  (define metalevel?
    (lambda (obj)
      (exact-integer? obj)))

  (define metalevel:syntax
    (lambda () -1))

  (define metalevel:run
    (lambda () 0))

  ;; Labels

  (define-record-type label
    (nongenerative label-a39e36ea-e0e4-4a27-9274-b982710361d8)
    (sealed #t)
    (fields
      (mutable binding)
      metalevel)
    (protocol
      (lambda (new)
        (define who 'make-label)
        (rec make
          (case-lambda
            [(bdg) (make bdg (metalevel:run))]
            [(bdg ml)
             (unless (binding? bdg)
               (assertion-violation who "invalid label argument" bdg))
             (unless (metalevel? ml)
               (assertion-violation who "invalid metalevel argument" ml))
             (new bdg ml)])))))

  (define/who label=?
    (lambda (l1 l2)
      (unless (label? l1)
        (assertion-violation who "invalid first label argument" l1))
      (unless (label? l2)
        (assertion-violation who "invalid second label argument" l2))
      (eq? l1 l2)))

  (define/who label-kill!
    (lambda (lbl)
      (unless (label? lbl)
        (assertion-violation who "invalid label argument" lbl))
      (label-binding-set! lbl (make-displaced-binding))))

  (define/who label->binding
    (lambda (lbl cml)
      (unless (or (not lbl) (label? lbl))
        (assertion-violation who "invalid labels argument" lbl))
      (unless (metalevel? cml)
        (assertion-violation who "invalid current metalevel argument" cml))
      (and lbl
	   (let ([bdg (label-binding lbl)])
             (if (in-phase? (label-metalevel lbl) cml)
                 bdg
                 (make-out-of-phase-binding))))))

  (define in-phase?
    (lambda (ml cml)
      (assert (metalevel? ml))
      (assert (metalevel? cml))
      (or (fx=? ml cml)
	  (and (fxnegative? ml)
	       (fx<=? (fxnot ml) cml)))))

  ;; Labels with props

  (define-record-type label/props
    (nongenerative label/props-b0f0a45a-a705-448c-b96f-ef8655d2b2b7)
    (sealed #t)
    (fields label props)
    (protocol
     (lambda (new)
       (define who 'make-label/props)
        (rec make
          (case-lambda
            [(lbl) (make lbl '())]
            [(lbl props)
             (unless (label? lbl)
               (assertion-violation who "invalid labels argument" lbl))
             (unless (and (list? props)
                          (for-all label? props))
               (assertion-violation who "invalid property list argument" props))
             (new lbl props)])))))

  (define/who label/props-merge
    (lambda (new-l/p prev-l/p)
      (define lbl
        (begin
          (unless (label/props? new-l/p)
            (assertion-violation who "invalid new label/props argument" new-l/p))
          (unless (label/props? prev-l/p)
            (assertion-violation who "invalid previous label/props argument" prev-l/p))
          (label/props-label new-l/p)))
      (if (label=? lbl (label/props-label prev-l/p))
	  (make-label/props lbl
			    (delete-duplicates
			     (append (label/props-props new-l/p)
				     (label/props-props prev-l/p))
                             label=?))
	  new-l/p)))

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

  ;; Ribs

  (define make-rib
    (lambda () (make-eq-hashtable)))

  (define rib?
    (lambda (x)
      (hashtable? x)))

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

  (define/who ribcage-add-barrier!
    (lambda (r mark-list*)
      (unless (ribcage? r)
        (assertion-violation who "invalid ribcage argument" r))
      (unless (and (list? mark-list*)
		   (for-all mark-list? mark-list*))
        (assertion-violation who "invalid list of mark lists argument" mark-list*))
      (ribcage-chunks-set!
       r
       (cons* (make-eq-hashtable)
	      (delete-duplicates mark-list* marks=?)
	      (ribcage-chunks r)))))

  (define/who ribcage-for-each
    (lambda (proc r)
      (unless (procedure? proc)
        (assertion-violation who "invalid procedure argument" proc))
      (unless (ribcage? r)
        (assertion-violation who "invalid ribcage argument" r))
      (let-values ([(n* a*) (hashtable-entries (car (ribcage-chunks r)))])
	(vector-for-each (lambda (n a)
			   (for-each (lambda (p)
				       (proc n (car p) (cdr p)))
				     a))
			 n* a*))))

  (define ass-marks
    (lambda (marks a)
      (assp (lambda (m) (marks=? m marks)) a)))

  (define rem-marks
    (lambda (marks a)
      (remp (lambda (p) (marks=? (car p) marks)) a)))

  (define-condition-type &duplicate-definition
    &condition
    make-duplicate-definition-condition duplicate-definition-condition?
    (name duplicate-definition-name))

  ;; Environments

  (define-record-type environment
    (nongenerative environment-3cd8d34b-252d-4240-8950-326edbf47a4f)
    (sealed #t)
    (fields rib))

  ;; Ribcages

  (define-record-type ribcage
    (nongenerative ribcage-4f79e972-eeda-436e-aa68-fff3f6ee27b2)
    (sealed #t)
    (fields (mutable chunks))
    (protocol
      (lambda (new)
        (define who 'make-ribcage)
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
             (new (list rib)))]))))

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

  ;; Substitutions

  (define-record-type shift
    (nongenerative shift-c350e6bd-e4e9-4ee2-bf70-804d264bff8c)
    (sealed #t))

  (define substitution?
    (lambda (obj)
      (or (shift? obj)
	  (ribcage? obj))))

  (define substitution-list?
    (lambda (obj)
      (and (list? obj)
           (for-all substitution? obj))))

  ;; Wraps

  (define-record-type wrap
    (nongenerative wrap-e7ee2eda-dd42-4089-a9ec-f6d97afc789c)
    (sealed #t)
    (fields marks substitutions)
    (protocol
      (lambda (new)
        (define who 'make-wrap)
        (lambda (m* s*)
          ;; Externally called?
          (unless (mark-list? m*)
            (assertion-violation who "invalid mark list argument" m*))
          (unless (substitution-list? s*)
            (assertion-violation who "invalid substitution list argument" s*))
          (new m* s*)))))

  (define/who join-wraps
    (lambda (w1 w2)
      (unless (wrap? w1)
        (assertion-violation who "invalid first wrap argument" w1))
      (unless (wrap? w2)
        (assertion-violation who "invalid first wrap argument" w2))
      (make-wrap
       (smart-append (wrap-marks w1) (wrap-marks w2))
       (smart-append (wrap-substitutions w1) (wrap-substitutions w2)))))

  ;; The idea for smart-append comes from Chez Scheme.

  (define smart-append
    (lambda (x y)
      (if (null? y) x (append x y))))

  ;; Syntax objects

  (define-record-type syntax-object
    (nongenerative syntax-object-b201d939-6db8-45be-a8a0-6ab49fff59e0)
    (fields expression wrap)
    (protocol
      (lambda (new)
        (define who 'make-syntax-object)
        (case-lambda
          [(expr wrap)
           ;; XXX: Who may call this syntax object?
           (unless (wrap? wrap)
             (assertion-violation who "invalid wrap argument" wrap))
           (new expr wrap)]
          [(expr)
           (new expr (make-wrap '() '()))]))))

  (define/who annotated-datum->syntax-object
    (lambda (annotation env)
      (unless (annotated-datum? annotation)
        (assertion-violation who "invalid annotation argument" annotation))
      (unless (environment? env)
        (assertion-violation who "invalid environment" env))
      (make-syntax-object annotation
                          (make-wrap '()
                                     (list (make-ribcage (environment-rib env)))))))

  (define/who syntax-object-source-location
    (lambda (stx)
      (define expr
        (begin
          (unless (syntax-object? stx)
            (assertion-violation who "invalid syntax object parameter" stx))
          (syntax-object-expression stx)))
      (and (annotated-datum? expr)
           (annotated-datum-source-location expr))))

  (define/who syntax-object-marks
    (lambda (stx)
      (unless (syntax-object? stx)
        (assertion-violation who "invalid syntax object argument" stx))
      (wrap-marks (syntax-object-wrap stx))))

  (define unwrap
    (lambda (stx)
      (if (syntax-object? stx)
          (syntax-object-expression stx)
          stx)))

  (define extend-wrap
    (lambda (x w)
      (if (syntax-object? x)
          (make-syntax-object
           (syntax-object-expression x)
           (join-wraps w (syntax-object-wrap x)))
          (make-syntax-object x w))))

  (define/who datum->syntax-object
    (lambda (tmpl datum)
      (unless ($identifier? tmpl)
        (assertion-violation who "invalid template argument" tmpl))
      (make-syntax-object (datum->annotated-datum datum) (syntax-object-wrap tmpl))))

  (define syntax-object->datum
    (lambda (stx)
      (define e (unwrap stx))
      (cond
       [(annotated-datum? e)
        (annotated-datum-value e)]
       [(pair? e)
        (cons (syntax-object->datum (car e))
              (syntax-object->datum (cdr e)))]
       [(vector? e)
        (vector-map syntax-object->datum e)]
       [else e])))

  (define syntax-atom?
    (lambda (stx)
      (annotated-atom? (unwrap stx))))

  (define syntax-null?
    (lambda (stx)
      (define expr (unwrap stx))
      (or (null? expr)
          (and (annotated-atom? expr)
               (null? (annotated-datum-value expr))))))

  (define syntax-pair?
    (lambda (stx)
      (annotated-pair? (unwrap stx))))

  (define/who syntax-car
    (lambda (stx)
      (unless (syntax-pair? stx)
        (assertion-violation who "invalid syntax pair argument" stx))
      (if (syntax-object? stx)
          (let ([w (syntax-object-wrap stx)]
                [e (annotated-pair-car (syntax-object-expression stx))])
            (extend-wrap e w))
          (car stx))))

  (define/who syntax-cdr
    (lambda (stx)
      (unless (syntax-pair? stx)
        (assertion-violation who "invalid syntax pair argument" stx))
      (if (syntax-object? stx)
          (let ([w (syntax-object-wrap stx)]
                [e (annotated-pair-cdr (syntax-object-expression stx))])
            (extend-wrap e w))
          (cdr stx))))

  (define syntax-vector?
    (lambda (stx)
      (let ([e (unwrap stx)])
        (or (vector? e)
            (and (annotated-vector? e))))))

  (define/who syntax-vector->list
    (lambda (stx)
      (unless (syntax-vector? stx)
        (assertion-violation who "invalid syntax vector argument" stx))
      (if (syntax-object? stx)
          (make-syntax-object (annotated-vector->list  (syntax-object-expression stx))
                              (syntax-object-wrap stx))
          (annotated-vector->list stx))))

  ;; Identifiers

  (define $identifier?
    (lambda (obj)
      (and (syntax-object? obj)
           (symbol? (syntax-object->datum obj)))))

  (define/who identifier->symbol
    (lambda (id)
      (unless ($identifier? id)
        (assertion-violation who "not an identifier" id))
      (syntax-object->datum id)))

  (define identifier-marks
    (lambda (id)
      (assert ($identifier? id))
      (syntax-object-marks id)))

  (define $bound-identifier=?
    (lambda (id1 id2)
      (define who 'bound-identifier=?)
      (unless ($identifier? id1)
	(assertion-violation who "not an identifier" id1))
      (unless ($identifier? id2)
	(assertion-violation who "not an identifier" id2))
      (and (symbol=? (identifier->symbol id1) (identifier->symbol id2))
	   (marks=? (identifier-marks id1) (identifier-marks id2)))))

  (define/who identifier->label
    (lambda (id)
      (unless ($identifier? id)
        (assertion-violation who "not an identifier" id))
      (let ([l/p (identifier->label/props id)])
	(and l/p (label/props-label l/p)))))

  (define/who identifier->label/props
    (lambda (id)
      (unless ($identifier? id)
        (assertion-violation who "not an identifier" id))
      (let ([sym (identifier->symbol id)]
	    [w (syntax-object-wrap id)])
	(let f ([m* (wrap-marks w)]
		[s* (wrap-substitutions w)])
	  (and (not (null? s*))
	       (if (shift? (car s*))
		   (f (cdr m*) (cdr s*))
		   (ribcage-ref (car s*)
			    sym
			    m*
			    (lambda ()
			      (f m* (cdr s*))))))))))

  (define $free-identifier=?
    (lambda (id1 id2)
      (unless ($identifier? id1)
	(assertion-violation 'free-identifier=? "not an identifier" id1))
      (unless ($identifier? id2)
	(assertion-violation 'free-identifier=? "not an identifier" id2))
      (let ([l1 (identifier->label id1)]
	    [l2 (identifier->label id2)])
	(if (or l1 l2)
	    (and l1 l2 (label=? l1 l2))
	    (symbol=? (identifier->symbol id1) (identifier->symbol id2))))))

  ;; Conditions

  (define-condition-type &syntax-error &error
    make-syntax-error syntax-error?
    (form syntax-error-form)
    (subform syntax-error-subform))

  ;; Core environment

  (define core-environment
    (make-environment (make-rib)))

  ;; Record writers

  (define-values (mark-counter! mark-count) (make-counter))

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
        (let ([name (mark-counter!)])
          (mark-name-set! r name)
          (put-string p (number->string name)))]
       [else (assert #f)])
      (put-string p ">")))

  (record-writer (record-type-descriptor shift)
    (lambda (r p wr)
      (put-string p "#<shift>")))

  (record-writer (record-type-descriptor syntax-object)
    (lambda (r p wr)
      (put-string p "#<syntax ")
      (wr (syntax-object->datum r) p)
      (put-string p ">")))

  (record-writer (record-type-descriptor environment)
    (lambda (r p wr)
      (put-string p "#<environment>")))
  )
