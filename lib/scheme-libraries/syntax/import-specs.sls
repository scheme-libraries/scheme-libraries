#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax import-specs)
  (export
    import-spec-import!)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries numbers)
    (scheme-libraries syntax-match)
    (scheme-libraries syntax-objects))

  (define/who import-spec-import!
    (lambda (imp-spec rib)
      (unless (rib? rib)
        (assertion-violation who "invalid rib argument" rib))
      (let ([imp-set (expand-import-spec imp-spec)])
        (rib-for-each! (lambda (name marks l/p)
                         (rib-set! rib name marks l/p))
                       imp-set))))

  (define expand-import-spec
    (lambda (spec)
      (let f ([imp-set (parse-import-spec form spec)])
        ...)

      ))

  ;; Parsers

  (define parse-import-spec
    (lambda (x)
      (syntax-match x
	[(for ,imp-set ,imp-lvl* ...)
	 (for-each
           (lambda (x)
	     (syntax-match x
	       [run #t]
	       [expand #t]
	       [(meta ,lvl) (guard (exact-integer? lvl)) #t]
	       [,x (syntax-error #f "invalid import level" #f x)]))
	   imp-lvl*)]
	[,x x])))

  (define parse-library-reference
    (lambda (x)
      (let-values ([(id* ver-ref) (split-library-reference x)])
        (values (map parse-library-name-part id*)
                (parse-version-reference ver-ref)))))

  (define split-library-reference
    (lambda (x)
      (syntax-match x
        [(,id* ... ,id)
         (guard (and (for-all $identifier? id*)))
         (values `(,id* ... ,id) '())]
        [(,id* ... ,ver-ref)
         (guard ($identifier? ver-ref))
         (values id* ver-ref)]
        [,x (syntax-error #f "ill-formed library reference" #f x)])))

  ;; FIXME TODO
  (define parse-version-reference
    (lambda (form stx)
      (let f ([stx stx])
        (syntax-match stx
          [(and ,ver-ref* ...)
           (let ([pred?* (map f ver-ref*)])
             (lambda (ver)
               (for-all (lambda (pred?) (pred? ver)) pred?*)))]
          [(or ,ver-ref* ...)
           (let ([pred?* (map f ver-ref*)])
             (lambda (ver)
               (exists (lambda (pred?) (pred? ver)) pred?*)))]
          [(not ,ver-ref)
           (let ([pred? (f ver-ref)])
             (lambda (ver)
               (not (pred? ver))))]
          [(,sub-ver-ref* ...)
           (let* ([pred?* (map (lambda (stx)
                                 (parse-sub-version-reference form stx))
                            sub-ver-ref*)]
                  [n (length pred?*)])
             (lambda (sub-ver*)
               (and (fx<=? n (length sub-ver*))
                    (for-all (lambda (pred? sub-ver) (pred? sub-ver)) pred?* sub-ver*))))]
          [,_
            (syntax-error #f "ill-formed version-reference" form stx)]))))

  ;; FIXME TODO
  (define parse-sub-version-reference
    (lambda (form stx)
      (let f ([stx stx])
	(syntax-match stx
	  [(>= ,sub-ver)
	   (let ([e (parse-sub-version form sub-ver)])
	     (lambda (ver) (>= e ver)))]
	  [(<= ,sub-ver)
	   (let ([e (parse-sub-version form sub-ver)])
	     (lambda (ver) (<= e ver)))]
	  [(and ,sub-ver-ref* ...)
	   (let ([pred?* (map f sub-ver-ref*)])
	     (lambda (ver)
	       (for-all (lambda (pred?) (pred? ver)) pred?*)))]
	  [(or ,sub-ver-ref* ...)
	   (let ([pred?* (map f sub-ver-ref*)])
	     (lambda (ver)
	       (exists (lambda (pred?) (pred? ver)) pred?*)))]
	  [(not ,sub-ver-ref)
	   (let ([pred? (f sub-ver-ref)])
	     (lambda (ver) (not (pred? ver))))]
	  [,_
           (let ([e (parse-sub-version form stx)])
	     (lambda (ver) (= ver e)))]))))


  )
