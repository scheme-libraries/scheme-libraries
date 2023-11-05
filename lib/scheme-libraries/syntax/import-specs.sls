#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax import-specs)
  (export
    import-spec-import!
    current-library-loader
    load-library)
  (import
    (rnrs)
    (scheme-libraries debug)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries numbers)
    (scheme-libraries parameters)
    (scheme-libraries symbols)
    (scheme-libraries syntax library-collections)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax $parsers)
    (scheme-libraries syntax $ribs)
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
    (case-lambda
      [(imp-spec rib)
       (import-spec-import! imp-spec rib (make-eq-hashtable))]
      [(imp-spec rib htimp)
       (unless (rib? rib)
         (assertion-violation who "invalid rib argument" rib))
       (unless (hashtable? htimp)
         (assertion-violation who "invalid imports argument" htimp))
       (let ([imp-set (expand-import-spec imp-spec htimp)])
         (rib-for-each (lambda (name marks l/p)
                         (rib-set! rib name marks l/p))
                       imp-set))]))

  (define expand-import-spec
    (lambda (spec htimp)
      (let f ([imp-set (parse-import-spec spec)])
        (syntax-match imp-set
          [(library ,lib-ref)
           (expand-library-reference lib-ref htimp)]
          [(only ,imp-set ,id* ...)
           (guard (for-all $identifier? id*))
           (only-import-set (f imp-set) id*)]
          [(except ,imp-set ,id* ...)
           (guard (for-all $identifier? id*))
           (except-import-set (f imp-set) id*)]
          [(prefix ,imp-set ,id)
           (guard ($identifier? id))
           (prefix-import-set (f imp-set) (identifier->symbol id))]
          [(rename ,imp-set (,orig-id* ,new-id*) ...)
           (guard (for-all $identifier? orig-id*) (for-all $identifier? new-id*))
           (rename-import-set imp-set (f imp-set) orig-id* new-id*)]
          [,imp-set (expand-library-reference imp-set htimp)]))))

  ;; prefix
  (define prefix-import-set
    (lambda (rib pre)
      (define new-rib (make-rib))
      (rib-for-each
       (lambda (n m l/p)
         (rib-set! new-rib (symbol-append pre n) m l/p))
       rib)
      new-rib))

  ;; only
  (define only-import-set
    (lambda (rib id*)
      (define new-rib (make-rib))
      (for-each
        (lambda (id)
          (define n (identifier->symbol id))
          (cond
           [(rib-ref rib n '())
            => (lambda (l/p)
                 (rib-set! new-rib n '() l/p))]
           [else
            (identifier-error #f id "identifier ~a not found in original set")]))
        id*)
      new-rib))

  ;; except
  (define except-import-set
    (lambda (rib id*)
      (define new-rib (make-rib))
      (define excepted (make-eq-hashtable))
      (for-each
        (lambda (id)
          (define n (identifier->symbol id))
          (unless (rib-ref rib n '())
            (identifier-error #f id "identifier ~a not found in original set"))
          (hashtable-set! excepted n #t))
        id*)
      (rib-for-each
       (lambda (n m l/p)
         (unless (hashtable-contains? excepted n)
           (rib-set! new-rib n m l/p)))
       rib)
      new-rib))

  ;; rename
  (define rename-import-set
    (lambda (imp-set rib orig-id* new-id*)
      (define new-rib (make-eq-hashtable))
      (define renames (make-eq-hashtable))
      (for-each
        (lambda (orig-id new-id)
          (define n (identifier->symbol orig-id))
          (unless (rib-ref rib n '())
            (identifier-error #f orig-id "identifier ~a not found in original set"))
          (hashtable-update! renames n
            (lambda (prev)
              (when prev
                (identifier-error #f orig-id "identifier ~a already renamed"))
              (identifier->symbol new-id))
            #f))
        orig-id* new-id*)
      (rib-for-each
       (lambda (n m l/p)
         (let ([n (hashtable-ref renames n n)])
           (rib-update! new-rib
                        n
                        '()
                        (lambda (prev)
                          (when prev
                            (syntax-error #f
                              (format "identifier ~a already in new set" n)
                              imp-set))
                          l/p)
                        #f)))
       rib)
      new-rib))

  (trace define expand-library-reference
    (lambda (lib-ref htimp)
      (let ([lib (import-library! lib-ref)])
        (when (library-uid lib) (hashtable-set! htimp lib #t))
        (library-exports lib))))

  (define import-library!
    (lambda (lib-ref)
      (or (maybe-import-library! lib-ref)
          (syntax-error #f (format "library ~a not found" (syntax-object->datum lib-ref))
            #f lib-ref))))

  (trace define maybe-import-library!
    (lambda (lib-ref)
      (define who 'import-library!)
      (let-values ([(name pred?) (parse-library-reference lib-ref)])
        (or (library-ref name #f)
            (begin
              (when (library-pending? name)
                (syntax-error #f "circular import of library" #f lib-ref))
              (library-pending! name #t)
              (let ([lib (load-library name pred?)])
                (library-set! name (or lib #t))
                (when (and lib (not (pred? (library-version lib))))
                  (syntax-error #f "library ~a version mismatch" #f lib-ref))
                (library-pending! name #f)
                lib))))))

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
	[,x x]))))
