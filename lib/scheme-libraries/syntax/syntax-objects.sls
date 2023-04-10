#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax syntax-objects)
  (export
    syntax-object?
    annotated-datum->syntax-object
    syntax-object-source-location)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (scheme-libraries counters)
    (scheme-libraries define-values)
    (scheme-libraries define-who)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries reading source-locations)
    (scheme-libraries record-writer))



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
    (lambda (annotation)
      (unless (annotated-datum? annotation)
        (assertion-violation who "invalid annotation argument" annotation))
      (make-syntax-object annotation)))

  (define/who syntax-object-source-location
    (lambda (stx)
      (define expr
        (begin
          (unless (syntax-object? stx)
            (assertion-violation who "invalid syntax object parameter" stx))
          (syntax-object-expression stx)))
      (and (annotated-datum? expr)
           (annotated-datum-source-location expr))))

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


  )
