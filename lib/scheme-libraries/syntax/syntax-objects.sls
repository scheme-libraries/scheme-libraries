#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax syntax-objects)
  (export
    make-location
    location?
    location-box
    location-box-set!
    location-name
    make-syntax-object
    syntax-object?
    syntax-object-wrap
    syntax-object-expression
    make-wrap
    wrap?
    wrap-marks
    wrap-substitutions
    substitution->s-exp
    s-exp->substitution
    annotated-datum->syntax-object
    syntax-object-source-location
    syntax-object-marks
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
    identifier->label/props
    ribcage-add!
    ribcage-add-property!
    ribcage
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
    make-application-type
    application-type?
    make-constant-type
    constant-type?
    constant-type-datum
    make-variable-binding
    variable-binding?
    variable-binding-library
    variable-binding-library-set!
    variable-binding-symbol
    variable-binding-location
    make-keyword-binding
    keyword-binding?
    keyword-binding-library
    keyword-binding-library-set!
    keyword-binding-transformer
    keyword-binding-transformer-set!
    make-property-binding
    property-binding?
    property-binding-library
    property-binding-library-set!
    property-binding-value
    property-binding-value-set!
    make-expander-binding
    expander-binding?
    expander-binding-proc
    make-splicing-binding
    splicing-binding?
    splicing-binding-proc
    make-auxiliary-binding
    auxiliary-binding?
    auxiliary-binding-name
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
    pattern-variable-binding-level
    property-set!)
  (import
    (except (rnrs) &syntax &undefined)
    (rnrs mutable-pairs)
    (scheme-libraries atoms)
    (scheme-libraries boxes)
    (scheme-libraries define-who)
    (scheme-libraries gensyms)
    (scheme-libraries helpers)
    (scheme-libraries lists)
    (scheme-libraries numbers)
    (scheme-libraries uuid)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries reading source-locations)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax $environments)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax $marks)
    (scheme-libraries syntax $metalevels)
    (scheme-libraries syntax $ribcages)
    (scheme-libraries syntax $syntax-types)
    (scheme-libraries syntax variables)
    (scheme-libraries syntax variable-transformers)
    (scheme-libraries parameters)
    (scheme-libraries rec)
    (scheme-libraries record-writer)
    (scheme-libraries void))

  ;; Locations

  (define-record-type location
    (nongenerative location-a10fb6e1-44cf-4c39-840f-14216ae9b94f)
    (fields name (mutable box))
    (protocol
      (lambda (new)
        (case-lambda
          [()
           (new (uid '%location) (void))]
          [(name)
           (new name (void))]))))

  ;; Bindings and syntax types

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
    (fields (mutable library) symbol location)
    (protocol
      (lambda (pargs->new)
        (define who 'make-variable-binding)
        (rec make
          (case-lambda
            [(var)
             (make var (make-location))]
            [(var loc)
             (unless (variable? var)
               (assertion-violation who "invalid variable argument" var))
             (unless (location? loc)
               (assertion-violation who "invalid location argument" loc))
             ((pargs->new) #f var loc)])))))

  (define-record-type keyword-binding
    (nongenerative keyword-binding-032ff78b-c673-47cd-9140-fc52de498e1a)
    (parent binding)
    (sealed #t)
    (fields (mutable library) (mutable transformer))
    (protocol
      (lambda (pargs->new)
        (define who 'make-keyword-binding)
        (lambda (proc)
          (unless (or (not proc)
                      (transformer? proc))
            (assertion-violation who "invalid procedure argument" proc))
          ((pargs->new) #f proc)))))

  (define-record-type property-binding
    (nongenerative property-binding-72417c6c-aee4-488a-8bba-fd364e5452ec)
    (parent binding)
    (sealed #t)
    (fields (mutable library) (mutable value))
    (protocol
      (lambda (pargs->new)
        (define who 'make-property-binding)
        (lambda (val)
          ((pargs->new) #f val)))))

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
    (fields name)
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

  (define substitution->s-exp
    (lambda (ribcage->symbol subst)
      (cond
       [(shift? subst) #f]
       [(ribcage? subst)
        (ribcage->symbol subst)]
       [else (assert #f)])))

  (define s-exp->substitution
    (lambda (symbol->ribcage e)
      (cond
       [(symbol? e) (symbol->ribcage e)]
       [(not e) (make-shift)]
       [else (assert #f)])))

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

  ;; Identifiers and ribcages

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
         (and (label=? lbl (identifier->label id)) ;test for
                                                   ;redefinition of
                                                   ;let(rec)-syntax-bound
                                                   ;keyword
              lbl))]
      [(ribs id bdg)
       (ribcage-add! ribs id bdg (current-metalevel))]))

  (define/who ribcage-add-property!
    (lambda (ribs id bdg lbl/props key-lbl)
       (unless (ribcage? ribs)
         (assertion-violation who "invalid ribcage argument" ribs))
       (unless ($identifier? id)
         (assertion-violation who "invalid identifier argument" id))
       (unless (binding? bdg)
         (assertion-violation who "invalid binding argument" bdg))
       (let ([lbl (label/props-label lbl/props)]
             [plbl (make-label bdg (current-metalevel-for-syntax))])
         (ribcage-set! ribs
                       (identifier->symbol id)
                       (syntax-object-marks id)
                       ;; TODO: Make the following more efficient.
                       (label/props-merge (make-label/props lbl (list (make-property key-lbl plbl)))
                                          lbl/props))
         (and (label=? lbl (identifier->label id))
              plbl))))

  (define/who ribcage
    (lambda (id* lbl*)
      (unless (and (list? id*)
                   (for-all $identifier? id*))
        (assertion-violation who "invalid identifier list argument" id*))
      (unless (and (list? lbl*)
                   (for-all label? lbl*))
        (assertion-violation who "invalid label list argument" lbl*))
      (make-ribcage (map identifier->symbol id*)
                    (map syntax-object-marks id*)
                    lbl*)))

  ;; Properties

  (define property-set!
    (lambda (label val)
      (assert (label? label))
      (let ([bdg (label-binding label)])
        (assert (property-binding? bdg))
        (property-binding-value-set! bdg val))))

  ;; Conditions

  (define-condition-type &syntax &error
    make-syntax-error syntax-error?
    (form syntax-error-form)
    (subform syntax-error-subform))

  (define-condition-type &undefined &error
    make-undefined-error undefined-error?)

  (define-condition-type &invalid-syntax-object
    &condition
    make-invalid-syntax-object-condition invalid-syntax-object-condition?
    (irritant invalid-syntax-object-irritant))

  ;; Exceptions

  (define invalid-syntax-object-condition
    (lambda (x)
      (raise (make-invalid-syntax-object-condition x))))

  ;; Record writers

  (record-writer (record-type-descriptor shift)
    (lambda (r p wr)
      (put-string p "#<shift>")))

  (record-writer (record-type-descriptor wrap)
    (lambda (r p wr)
      (put-string p "#<wrap ")
      (wr (wrap-marks r) p)
      (put-string p " ")
      (wr (wrap-substitutions r) p)
      (put-string p ">"))
    )

  (record-writer (record-type-descriptor syntax-object)
    (lambda (r p wr)
      (put-string p "#<syntax ")
      (wr (syntax-object->datum r) p)
      (put-string p ">")))
)
