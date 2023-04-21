#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax library-loaders)
  (export
    make-default-library-loader)
  (import
    (rnrs)
    (scheme-libraries parameters)
    (scheme-libraries thread-parameters)
    (scheme-libraries define-who)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax expand)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax library-locators)
    (scheme-libraries syntax $parsers)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects))

  ;; Library loaders

  (define make-default-library-loader
    (lambda (loc)
      (lambda (name pred?)
        (library-locator-search
          loc
          name pred?
          (lambda (e k)
            (let ([x (annotated-datum->syntax-object e (system-environment))])
              (let-values ([(n ver exp* imp* body*)
                            (parse-library-definition x)])
                (unless (and (library-name=? name n) (pred? ver))
                  (k))
                (let-values ([(lib)
                              (parameterize ([current-form x])
                                (expand-library name ver exp* imp* body*))])
                  ;; FIXME
                  #;
                  (record-expanded-library! lib)
                  #;
                  (library-bind-globals! lib lbl*)
                  lib))))
          (lambda ()
            #f)))))

  (define parse-library-definition
    (lambda (x)
      (parameterize ([current-form x])
        (syntax-match x
          [(library ,name (export ,exp-spec* ...) (import ,imp-spec*) ,body* ...)
           (let-values ([(name ver) (parse-library-name name)])
             (let ([ribs (make-ribcage)])
               (ribcage-add-barrier! ribs (list))
               (values name ver exp-spec* imp-spec*
                       (add-substitutions* ribs body*))))]
          [,x (syntax-error #f "invalid library definition" x)]))))

  (define parse-library-name
    (lambda (x)
      (define return
        (lambda (part* sub-ver*)
          (values (map parse-library-name part*)
                  (map parse-sub-version sub-ver*))))
      (syntax-match x
        [(,part* ... (,sub-ver* ...))
         (for-all $identifier? part*)
         (return part* sub-ver*)]
        [(,part* ...)
         (for-all $identifier? part*)
         (return part* '())]
        [,x (syntax-error #f "ill-formed library name" #f x)])))

  )
