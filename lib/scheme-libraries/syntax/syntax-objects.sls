#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax syntax-objects)
  (export
    metalevel?
    metalevel:syntax
    metalevel:run
    metalevel-for-syntax
    current-metalevel
    current-metalevel-for-syntax
    make-label
    label?
    label-bind!
    label-kill!
    label->binding
    make-label/props
    make-environment
    environment?
    environment-set!
    make-ribcage
    ribcage?
    ribcage-add!
    syntax-object?
    annotated-datum->syntax-object
    syntax-object-source-location
    add-substitutions
    add-substitutions*
    syntax-object->datum
    datum->syntax-object
    generate-temporary
    syntax-atom?
    syntax-null?
    syntax-pair?
    syntax-car
    syntax-cdr
    syntax-length+
    syntax-split
    syntax-vector?
    syntax-list
    syntax-vector->list
    syntax-list->vector
    make-mark
    apply-anti-mark
    wrap-syntax-object
    $identifier?
    $bound-identifier=?
    $free-identifier=?
    identifier->symbol
    identifier->label
    (rename (&syntax $&syntax))
    (rename (&undefined $&undefined))
    make-syntax-error
    syntax-error?
    syntax-error-form
    syntax-error-subform
    &invalid-syntax-object
    make-invalid-syntax-object-condition
    invalid-syntax-object-condition?
    invalid-syntax-object-irritant
    make-other-type
    other-type?
    make-displaced-binding
    displaced-binding?
    make-out-of-phase-binding
    out-of-phase-binding?
    make-application-type
    application-type?
    make-constant-type
    constant-type?
    constant-type-datum
    make-variable-binding
    variable-binding?
    variable-binding-symbol
    make-keyword-binding
    keyword-binding?
    keyword-binding-transformer
    make-expander-binding
    expander-binding?
    expander-binding-proc
    make-splicing-binding
    splicing-binding?
    splicing-binding-proc
    make-auxiliary-binding
    auxiliary-binding?
    auxiliary-binding-who
    make-definition-binding
    definition-binding?
    definition-binding-proc
    make-prim-binding
    prim-binding?
    prim-binding-name
    prim-binding-arity
    make-pattern-variable-binding
    pattern-variable-binding?
    pattern-variable-binding-identifier
    pattern-variable-binding-level)
  (import
    (except (rnrs) &syntax &undefined)
    (rnrs mutable-pairs)
    (scheme-libraries atoms)
    (scheme-libraries counters)
    (scheme-libraries define-values)
    (scheme-libraries define-who)
    (scheme-libraries gensyms)
    (scheme-libraries helpers)
    (scheme-libraries lists)
    (scheme-libraries numbers)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries reading source-locations)
    (scheme-libraries syntax variables)
    (scheme-libraries syntax variable-transformers)
    (scheme-libraries parameters)
    (scheme-libraries rec)
    (scheme-libraries record-writer))

  ;; Bindings and syntax types

  (define-record-type syntax-type
    (nongenerative syntax-type-5076a7a5-0cce-49cb-9928-256ffdfa7aee))

  (define-record-type binding
    (nongenerative binding-749b4948-3923-484f-b466-3e8a9048a931)
    (parent syntax-type))

  (define-record-type displaced-binding
    (nongenerative displaced-binding-ac5ee8c9-fc00-4e4d-919e-90373972d283)
    (parent binding) (sealed #t))

  (define-record-type out-of-phase-binding
    (nongenerative out-of-phase-binding-9f0102df-6804-4d78-b919-27057da676bc)
    (parent binding)
    (sealed #t))

  (define-record-type other-type
    (nongenerative other-binding-5da819c4-d149-4c12-b13f-949a991ca3f4)
    (parent syntax-type) (sealed #t))

  (define-record-type application-type
    (nongenerative application-binding-5faa5a54-ef82-4a84-b402-01f6b91a96a4)
    (parent syntax-type) (sealed #t))

  (define-record-type constant-type
    (nongenerative constant-binding-731a5c50-504c-494f-b791-e6819432b70c)
    (parent syntax-type)
    (sealed #t)
    (fields datum)
    (protocol
      (lambda (pargs->new)
        (define who 'make-constant-binding)
        (lambda (datum)
          (unless (constant? datum)
            (assertion-violation who "invalid constant argument" datum))
          ((pargs->new) datum)))))

  (define-record-type expander-binding
    (nongenerative expander-binding-788e9954-aa65-42d0-a43f-381ab32d326f)
    (parent binding)
    (sealed #t)
    (fields proc)
    (protocol
      (lambda (pargs->new)
        (define who 'make-expander-binding)
        (lambda (proc)
          (unless (procedure? proc)
            (assertion-violation who "invalid procedure argument" proc))
          ((pargs->new) proc)))))

  (define-record-type splicing-binding
    (nongenerative splicing-binding-76a8d6ec-6395-4e48-9800-ca8a48ff9f87)
    (parent binding)
    (sealed #t)
    (fields proc)
    (protocol
      (lambda (pargs->new)
        (define who 'make-splicing-binding)
        (lambda (proc)
          (unless (procedure? proc)
            (assertion-violation who "invalid procedure argument" proc))
          ((pargs->new) proc)))))

  (define-record-type variable-binding
    (nongenerative variable-binding-d1b200da-754e-43ec-86bf-d03cd03c0da1)
    (parent binding)
    (sealed #t)
    (fields symbol)
    (protocol
      (lambda (pargs->new)
        (define who 'make-variable-binding)
        (lambda (var)
          (unless (variable? var)
            (assertion-violation who "invalid variable argument" var))
          ((pargs->new) var)))))

  (define-record-type keyword-binding
    (nongenerative keyword-binding-032ff78b-c673-47cd-9140-fc52de498e1a)
    (parent binding)
    (sealed #t)
    (fields transformer)
    (protocol
      (lambda (pargs->new)
        (define who 'make-keyword-binding)
        (lambda (proc)
          (unless (or (procedure? proc)
                      (variable-transformer? proc))
            (assertion-violation who "invalid procedure argument" proc))
          ((pargs->new) proc)))))

  (define-record-type definition-binding
    (nongenerative definition-binding-549adafc-af54-45da-b1a8-fa63c6e2ce19)
    (parent binding) (sealed #t)
    (fields proc)
    (protocol
      (lambda (pargs->new)
        (define who 'make-definition-binding)
        (lambda (proc)
          (unless (procedure? proc)
            (assertion-violation who "invalid procedure argument" proc))
          ((pargs->new) proc)))))

  (define-record-type auxiliary-binding
    (nongenerative auxiliary-binding-90472bac-e875-4218-bc1c-f856e1587073)
    (parent binding) (sealed #t)
    (fields who)
    (protocol
      (lambda (pargs->new)
        (define who 'make-auxiliary-binding)
        (lambda (name)
          (unless (symbol? name)
            (assertion-violation who "invalid who argument" name))
          ((pargs->new) name)))))

  (define-record-type prim-binding
    (nongenerative prim-binding-676291b2-9e1c-4044-b1be-7c27030ddb0f)
    (parent binding) (sealed #t)
    (fields name arity)
    (protocol
      (lambda (pargs->new)
        (define who 'make-prim-binding)
        (lambda (name arity)
          (unless (symbol? name)
            (assertion-violation who "invalid primitive argument" name))
          (unless (fixnum? arity)
            (assertion-violation who "invalid arity argument" arity))
          ((pargs->new) name arity)))))

  (define-record-type pattern-variable-binding
    (nongenerative pattern-variable-binding-35069673-4c20-44d1-a6d0-b8e45243ade0)
    (parent binding) (sealed #t)
    (fields identifier level)
    (protocol
      (lambda (pargs->new)
        (define who 'make-pattern-variable-binding)
        (lambda (id lvl)
          (unless ($identifier? id)
            (assertion-violation who "invalid identifier argument" id))
          (unless (nonnegative-fixnum? lvl)
            (assertion-violation who "invalid level argument" lvl))
          ((pargs->new) id lvl)))))

  ;; Metalevels

  (define metalevel?
    (lambda (obj)
      (exact-integer? obj)))

  (define metalevel-for-syntax
    (lambda (ml)
      (fxnot ml)))

  (define metalevel:syntax
    (lambda () -1))

  (define metalevel:run
    (lambda () 0))

  (define/who current-metalevel
    (make-parameter (metalevel:run)
      (lambda (x)
        (unless (metalevel? x)
          (assertion-violation who "invalid metalevel" x))
        x)))

  (define current-metalevel-for-syntax
    (lambda ()
      (metalevel-for-syntax (current-metalevel))))

  ;; Label

  (define-record-type label
    (nongenerative label-a39e36ea-e0e4-4a27-9274-b982710361d8)
    (sealed #t)
    (fields
      (mutable binding)
      metalevel
      (mutable name))
    (protocol
      (lambda (new)
        (define who 'make-label)
        (rec make
          (case-lambda
            [()
             (make #f (current-metalevel) #f)]
            [(bdg)
             (make bdg (current-metalevel) #f)]
            [(bdg ml)
             (make bdg ml #f)]
            [(bdg ml name)
             (let ([bdg (or bdg (make-displaced-binding))])
               (unless (binding? bdg)
                 (assertion-violation who "invalid label argument" bdg))
               (unless (metalevel? ml)
                 (assertion-violation who "invalid metalevel argument" ml))
               (unless (or (not name)
                           (symbol? name))
                 (assertion-violation who "invalid symbol argument" name))
               (new bdg ml name))])))))

  (define/who label=?
    (lambda (l1 l2)
      (unless (label? l1)
        (assertion-violation who "invalid first label argument" l1))
      (unless (label? l2)
        (assertion-violation who "invalid second label argument" l2))
      (eq? l1 l2)))

  (define/who label-bind!
    (lambda (lbl bdg)
      (unless (label? lbl)
        (assertion-violation who "invalid label argument" lbl))
      (unless (binding? bdg)
        (assertion-violation who "invalid binding argument" bdg))
      (label-binding-set! lbl bdg)))

  (define/who label-kill!
    (lambda (lbl)
      (unless (label? lbl)
        (assertion-violation who "invalid label argument" lbl))
      (label-binding-set! lbl (make-displaced-binding))))

  (define/who label->binding
    (lambda (lbl)
      (unless (or (not lbl) (label? lbl))
        (assertion-violation who "invalid labels argument" lbl))
      (and lbl
	   (let ([bdg (label-binding lbl)])
             (if (in-phase? (label-metalevel lbl)
                            (current-metalevel))
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

  (define/who ribcage-add!
    (case-lambda
      [(ribs id bdg ml)
       (unless (ribcage? ribs)
         (assertion-violation who "invalid ribcage argument" ribs))
       (unless ($identifier? id)
         (assertion-violation who "invalid identifier argument" id))
       (unless (binding? bdg)
         (assertion-violation who "invalid binding argument" bdg))
       (let ([lbl (make-label bdg ml)])
         (ribcage-set! ribs
                       (identifier->symbol id)
                       (syntax-object-marks id)
                       (make-label/props lbl))
         (and (label=? lbl (identifier->label id))
              lbl))]
      [(ribs id bdg)
       (ribcage-add! ribs id bdg (current-metalevel))]))

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

  ;; Environments

  (define-record-type environment
    (nongenerative environment-3cd8d34b-252d-4240-8950-326edbf47a4f)
    (sealed #t)
    (fields rib)
    (protocol
      (lambda (new)
        (lambda ()
          (new (make-rib))))))

  (define/who environment-set!
    (lambda (env name l/p)
      (unless (environment? env)
        (assertion-violation who "invalid environment argument" env))
      (unless (symbol? name)
        (assertion-violation who "invalid name argument" name))
      (unless (label/props? l/p)
        (assertion-violation who "invalid label/props argument" name))
      (rib-set! (environment-rib env) name '() l/p)))

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
            [(id* lbl*)
             (unless (and (list? id*)
                          (for-all $identifier? id*))
               (assertion-violation who "invalid identifier list argument" id*))
             (unless (and (list? lbl*)
                          (for-all label? lbl*))
               (assertion-violation who "invalid label list argument" lbl*))
             (make (map identifier->symbol id*)
                   (map syntax-object-marks id*)
                   lbl*)]
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

  (define/who add-substitutions
    (lambda (s x)
      (unless (substitution? s)
        (assertion-violation who "invalid substitutions argument" s))
      (extend-wrap x (make-wrap '() (list s)))))

  (define/who add-substitutions*
    (lambda (s x*)
      (unless (substitution? s)
        (assertion-violation who "invalid substitutions argument" s))
      (unless (list? x*)
        (assertion-violation who "invalid syntax list argument" x*))
      (map (lambda (x) (add-substitutions s x)) x*)))

  (define/who syntax-object-marks
    (lambda (stx)
      (unless (syntax-object? stx)
        (assertion-violation who "invalid syntax object argument" stx))
      (wrap-marks (syntax-object-wrap stx))))

  (define apply-anti-mark
    (lambda (x)
      (extend-wrap x (make-wrap (list (anti-mark)) (list (make-shift))))))

  (define apply-wrap
    (lambda (x mark ribs)
      (let ([w (let* ([w (syntax-object-wrap x)]
                      [m* (wrap-marks w)]
                      [s* (wrap-substitutions w)])
                 (if (and (pair? m*)
                          (anti-mark? (car m*)))
                     (make-wrap (cdr m*)
                                (cdr s*))
                     (make-wrap (cons mark m*)
                                (if ribs
                                    (cons* ribs (make-shift) s*)
                                    (cons (make-shift) s*)))))])
        (make-syntax-object (syntax-object-expression x) w))))

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

  (define generate-temporary
    (lambda ()
      (make-syntax-object (gensym))))

  (define syntax-atom?
    (lambda (stx)
      (annotated-atom? (unwrap stx))))

  (define syntax-null?
    (lambda (stx)
      (define expr (unwrap stx))
      (or (null? expr)
          (and (annotated-datum? expr)
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

  (define/who syntax-length+
    (lambda (stx)
      ;; TODO: Raise an error is stx is not a valid syntax object (has
      ;; circular structure).
      (let f ([stx stx] [n 0])
        (cond
         [(syntax-pair? stx)
          (f (syntax-cdr stx)
             (fx+ n 1))]
         [else n]))))

  (define syntax-split
    (lambda (x k succ fail)
      (let ([n (syntax-length+ x)])
        (if (fx<=? k n)
            (call-with-values
                (lambda ()
                  (syntax-split-at x (fx- n k)))
              succ)
            (fail)))))

  (define syntax-split-at
    (lambda (x k)
      (let f ([x x] [k k])
        (cond
         [(fxzero? k) (values '() x)]
         [(syntax-pair? x)
          (let-values ([(x1 x2) (f (syntax-cdr x) (fx- k 1))])
            (values (cons (syntax-car x) x1) x2))]
         [else (assert #f)]))))

  (define syntax-list
    (lambda (stx)
      (let f ([x stx])
        (cond
         [(syntax-pair? x)
          (cons (syntax-car x)
                (f (syntax-cdr x)))]
         [(syntax-null? x) '()]
         [else
          (assertion-violation 'quasisyntax "attempt to splice an non-list" stx)]))))

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

  (define syntax-list->vector
    (lambda (stx)
      (let ([n (syntax-length+ stx)])
        (do ([v (make-vector n)]
             [i 0 (fx+ i 1)]
             [stx stx (syntax-cdr stx)])
            ((fx=? i n) v)
          (vector-set! v i (syntax-car stx))))))

  (define wrap-syntax-object
    (lambda (x mark ribs)
      (let ([x (build-syntax-object x mark ribs)])
        (if (syntax-object? x)
            x
            (make-syntax-object x)))))

  (define build-syntax-object
    (lambda (x mark ribs)
      (let ([ht (make-eq-hashtable)])
        (let f ([x x])
          (cond
           [(hashtable-ref ht x #f)
            (invalid-syntax-object-condition x)]
           [(pair? x)
            (cons (f (car x)) (f (cdr x)))]
           [(vector? x)
            (vector-map f x)]
           [(symbol? x)
            (invalid-syntax-object-condition x)]
           [(syntax-object? x)
            (apply-wrap x mark ribs)]
           [(atom? x) x]
           [else
            (invalid-syntax-object-condition x)])))))

  ;; Identifiers

  (define $identifier?
    (lambda (obj)
      (and (syntax-object? obj)
           (symbol? (syntax-object->datum obj)))))

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

  (define/who identifier->symbol
    (lambda (id)
      (unless ($identifier? id)
        (assertion-violation who "not an identifier" id))
      (syntax-object->datum id)))

  ;; Conditions

  (define-condition-type &syntax &error
    make-syntax-error syntax-error?
    (form syntax-error-form)
    (subform syntax-error-subform))

  (define-condition-type &undefined &error
    make-undefined-error undefined-error?)

  (define-condition-type &duplicate-definition
    &condition
    make-duplicate-definition-condition duplicate-definition-condition?
    (name duplicate-definition-name))

  (define-condition-type &invalid-syntax-object
    &condition
    make-invalid-syntax-object-condition invalid-syntax-object-condition?
    (irritant invalid-syntax-object-irritant))

  ;; Exceptions

  (define invalid-syntax-object-condition
    (lambda (x)
      (raise (make-invalid-syntax-object-condition x))))

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
      (put-string p "#<environment>"))))
