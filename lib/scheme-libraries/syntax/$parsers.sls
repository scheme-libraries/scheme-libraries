#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $parsers)
  (export
    parse-import-form
    parse-library-reference
    parse-library-name-part
    parse-sub-version)
  (import
    (rnrs)
    (scheme-libraries lists)
    (scheme-libraries numbers)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects))

  (define parse-import-form
    (lambda (x)
      (syntax-match x
	[(import ,imp* ...) imp*]
	[,_
	 (syntax-error #f "invalid import form" x)])))

  (define parse-library-name-part
    (lambda (x)
      (unless ($identifier? x)
        (syntax-error #f "invalid library name part" #f x))
      (identifier->symbol x)))

  (define parse-sub-version
    (lambda (x)
      (let ([e (syntax-object->datum x)])
        (unless (exact-nonnegative-integer? e)
          (syntax-error #f "invalid library sub-version" #f x))
        e)))

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
                    (for-all (lambda (pred? sub-ver) (pred? sub-ver))
                             pred?*
                             (take sub-ver* n)))))]
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
