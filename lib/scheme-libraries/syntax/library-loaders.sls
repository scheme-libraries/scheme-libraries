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
    (scheme-libraries syntax library-collections)
    (scheme-libraries syntax library-locators)
    (scheme-libraries syntax $metalevels)
    (scheme-libraries syntax $parsers)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects))

  ;; Library loaders

  (define make-default-library-loader
    (lambda (loc)
      (lambda (name pred?)
        (call/cc
         (lambda (return)
           (library-locator-for-each
            (lambda (e)
              (let ([x (annotated-datum->syntax-object e (system-environment))])
                (let-values ([(n ver exp* imp* body*)
                              (parse-library-definition x)])
                  ;(display n) (newline)
                  (when (and (library-name=? name n) (pred? ver))
                    (let-values ([(lib lbl*)
                                  ;; XXX: Can we extract lbl* from lib?
                                  (parameterize ([current-form x])
                                    (expand-library name ver exp* imp* body*))])
                      (library-list-append! lib)
                      (library-bind-globals! lib lbl*)
                      (return lib))))))
            loc name pred?)
           #f)))))

  (define library-bind-globals!
    (lambda (lib lbl*)
      (define dobind!
        (lambda (lbl)
          (label-metalevel-set! lbl (metalevel:syntax))
          (let ([bdg (label->binding lbl)])
            (cond
             [(variable-binding? bdg)
              (variable-binding-library-set! bdg lib)]
             [(keyword-binding? bdg)
              (keyword-binding-library-set! bdg lib)]
             [(auxiliary-binding? bdg)]
             [else
              ;; Only variables, keywords, and auxiliary syntax should
              ;; be defineable at the top level of a library.
              (assert #f)]))))
      (for-each dobind! lbl*)))

  ;; Parsers

  (define parse-library-definition
    (lambda (x)
      (parameterize ([current-form x])
        (syntax-match x
          [(library ,name (export ,exp-spec* ...) (import ,imp-spec* ...) ,body* ...)
           (let-values ([(name ver) (parse-library-name name)])
             (values name ver exp-spec* imp-spec* body*))]
          [,x (syntax-error #f "invalid library definition" x)]))))

  (define parse-library-name
    (lambda (x)
      (define return
        (lambda (part* sub-ver*)
          (values (map parse-library-name-part part*)
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
