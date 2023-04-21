#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax import-specs)
  (export
    import-spec-import!
    current-library-loader)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries numbers)
    (scheme-libraries parameters)
    (scheme-libraries syntax library-collections)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax $parsers)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects))

  ;; Library loaders

  (define/who current-library-loader
    (make-parameter
        (lambda (name pred?)
          (assertion-violation 'load-library "no library loader installed" name pred?))
      (lambda (loader)
        (unless (procedure? loader)
          (assertion-violation who "invalid library loader" loader))
        loader)))

  (define/who load-library
    (lambda (name pred?)
      ((current-library-loader) name pred?)))

  ;; Importing

  (define/who import-spec-import!
    (lambda (imp-spec rib)
      (unless (rib? rib)
        (assertion-violation who "invalid rib argument" rib))
      (let ([imp-set (expand-import-spec imp-spec)])
        (rib-for-each (lambda (name marks l/p)
                        (rib-set! rib name marks l/p))
                      imp-set))))

  (define expand-import-spec
    (lambda (spec)
      (let f ([imp-set (parse-import-spec spec)])
        (syntax-match imp-set
          [(library ,lib-ref)
           (expand-library-reference lib-ref)]
          [(only ,imp-set ,id* ...)
           (guard (for-all $identifier? id*))
           ;; FIXME
           (assert #f)]
          [(except ,imp-set ,id* ...)
           (guard (for-all $identifier? id*))
           ;; FIXME
           (assert #f)]
          [(prefix ,imp-set ,id)
           (guard ($identifier? id))
           ;; FIXME
           (assert #f)]
          [(rename ,imp-set (,orig-id* ,new-id*) ...)
           (guard (for-all $identifier? orig-id*) (for-all $identifier? new-id*))
           ;; FIXME
           (assert #f)]
          [,imp-set (expand-library-reference imp-set)]))))

  (define expand-library-reference
    (lambda (lib-ref)
      (library-exports (import-library! lib-ref))))

  (define import-library!
    (lambda (lib-ref)
      (or (maybe-import-library! lib-ref)
          (syntax-error #f (format "library ~a not found" (syntax-object->datum lib-ref))
            #f lib-ref))))

  (define maybe-import-library!
    (lambda (lib-ref)
      (let-values ([(name pred?) (parse-library-reference lib-ref)])
        (let ([lib (library-ref name #f)])
          (or lib
              (begin
                (when (library-pending? name)
                  (syntax-error #f "circular import of library" #f lib-ref))
                (library-pending! name #t)
                (let ([lib (load-library name pred?)])
                  (library-set! name (or lib #t))
                  (when (and lib (not (pred? (library-version lib))))
                    (syntax-error #f "library ~a version mismatch" #f lib-ref))
                  (library-pending! name #f)
                  lib)))))))

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

  (define parse-version-reference
    (lambda (x)
      (let f ([x x])
        (syntax-match x
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
           (let* ([pred?* (map parse-sub-version-reference sub-ver-ref*)]
                  [n (length pred?*)])
             (lambda (sub-ver*)
               (and (fx<=? n (length sub-ver*))
                    (for-all (lambda (pred? sub-ver) (pred? sub-ver)) pred?* sub-ver*))))]
          [,x
            (syntax-error #f "ill-formed version-reference" #f x)]))))

  (define parse-sub-version-reference
    (lambda (x)
      (let f ([x x])
	(syntax-match x
	  [(>= ,sub-ver)
	   (let ([e (parse-sub-version sub-ver)])
	     (lambda (ver) (>= e ver)))]
	  [(<= ,sub-ver)
	   (let ([e (parse-sub-version sub-ver)])
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
	  [,x
           (let ([e (parse-sub-version x)])
	     (lambda (ver) (= ver e)))]))))


  )
