#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expand)
  (export
    expand
    expand-body
    expand-expression
    expand-library
    transform)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries helpers)
    (scheme-libraries lists)
    (scheme-libraries match)
    (scheme-libraries modules)
    (scheme-libraries numbers)
    (scheme-libraries parameters)
    (scheme-libraries uuid)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries syntax $environments)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax $ribs)
    (scheme-libraries syntax $ribcages)
    (except
        (scheme-libraries syntax $syntax-types)
      syntax-type)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax import-specs)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects)
    (scheme-libraries syntax variables)
    (scheme-libraries syntax variable-transformers))

  ;; Expansion modes

  (define-enumeration expansion-mode
    (body library program script)
    expansion-modes)

  (define current-mode
    (make-parameter (expansion-mode body)
      (lambda (mode)
        (assert (enum-set-member? mode (enum-set-universe (expansion-modes))))
        mode)))

  (define top-level-mode?
    (lambda ()
      (enum-set-member? (current-mode)
                        (expansion-modes library program script))))

  (define library-mode?
    (lambda ()
      (enum-set-member? (current-mode)
                        (expansion-modes library))))

  ;; Expander

  (define/who expand
    (lambda (expr env)
      (unless (annotated-datum? expr)
        (assertion-violation who "invalid expression argument" expr))
      (unless (environment? env)
        (assertion-violation who "invalid environment argument" env))
      (expand-expression (annotated-datum->syntax-object expr env))))

  (define expand-expression
    (lambda (x)
      (let f ([x x])
        (let-values ([(x t) (syntax-type x #f)])
          (cond
           [(application-type? t)
            (expand-application x)]
           [(splicing-binding? t)
            (expand-splicing-expression x t)]
           [(expander-binding? t)
            ((expander-binding-proc t) x)]
           [(constant-type? t)
            (build (quote ,(constant-type-datum t)))]
           [(prim-binding? t)
            (expand-primop t x)]
           [(variable-binding? t)
            (cond
             [(variable-binding-library t)
              => (lambda (lib)
                   (let ([var (variable-binding-symbol t)])
                     (require-for-runtime! lib var (assert (identifier->label x)))
                     (build ,var)))]
             [else
              (build ,(variable-binding-symbol t))])]
           [else
            (display x) (newline)
            (syntax-error #f "invalid syntax in expression context" x)])))))

  (define expand-application
    (lambda (x)
      (syntax-match x
        [(,[expand-expression -> e] ,[expand-expression -> e*] ...)
         (build (,e ,e* ...))]
        [,x (syntax-error #f "invalid application syntax" x)])))

  (define expand-splicing-expression
    (lambda (x t)
      (let ([x* ((splicing-binding-proc t) x)])
        (if (null? x*)
            (syntax-error #f "empty body" x)
            (build-begin ,(map expand-expression x*) ...)))))

  (define expand-begin-expression
    (lambda (x)
      (syntax-match x
        [(,k ,[expand-expression -> e*] ... ,[expand-expression -> e])
         (build-begin ,e* ... ,e)]
        [,x (syntax-error 'begin "invalid syntax" x)])))

  (define expand-primop
    (lambda (t x)
      (syntax-match x
        [(,k ,[expand-expression -> e*] ...)
         (let ([arity (prim-binding-arity t)]
               [name (prim-binding-name t)]
               [n (length e*)])
           (if (fxnegative? arity)
               (unless (fx>=? n (fxnot arity))
                 (syntax-error name
                   "insufficient number of arguments" e*))
               (unless (fx=? n arity)
                 (syntax-error name
                   "wrong number of arguments" e*)))
           (build (,name ,e* ...)))])))

  (define expand-body
    (lambda (x*)
      (let-values ([(viscmd* def* e lbl*)
                    (expand-internal x* (make-ribcage) (expansion-mode body))])
        (if (null? def*)
            e
            (let ([x* (map definition-var def*)]
                  [e* (map definition-expr def*)])
              (build (letrec* ([,x* ,e*] ...) ,e)))))))

  (define expand-internal
    (lambda (x* ribs mode)
      (let ([x* (add-substitutions* ribs x*)])
        (parameterize ([current-mode mode])
          (expand-form* x* ribs '() '() '())))))

  (define expand-form*
    (lambda (x* ribs rviscmd* rdef* lbl*)
      (match x*
        [()
         (unless (top-level-mode?)
           (syntax-error #f "no expressions in body"))
         (values (reverse rviscmd*) (expand-definitions (reverse rdef*)) #f lbl*)]
        [(,x . ,x*)
         (expand-form x x* ribs rviscmd* rdef* lbl*)])))

  (define expand-form
    (lambda (x x* ribs rviscmd* rdef* lbl*)
      (let-values ([(x t) (syntax-type x ribs)])
        (cond
         [(definition-binding? t)
          (let-values ([(viscmd* def* nlbl*) ((definition-binding-proc t) x ribs)])
            (expand-form* x* ribs
                          (append (reverse viscmd*) rviscmd*)
                          (append (reverse def*) rdef*)
                          (append nlbl* lbl*)))]
         [(splicing-binding? t)
          (expand-splicing x t x* ribs rviscmd* rdef* lbl*)]
         [(library-mode?)
          (expand-form* '() ribs
                        rviscmd*
                        (cons
                         (lambda ()
                           (make-command
                            (expand-expression
                             (syntax-extend-backquote here
                               `(begin ,x ,x* ... (void))))))
                         rdef*)
                        lbl*)]
         [else
          (let ([e* (map expand-expression (cons x x*))])
            (values (reverse rviscmd*)
                    (expand-definitions (reverse rdef*))
                    (build-begin ,e* ...)
                    lbl*))]))))

  (define expand-splicing
    (lambda (x t x* ribs rviscmd* rdef* lbl*)
      (let ([e* ((splicing-binding-proc t) x)])
        (expand-form* (append e* x*) ribs rviscmd* rdef* lbl*))))

  (define expand-definitions
    (lambda (def*)
      (map (lambda (thunk) (thunk)) def*)))

  (define make-command
    (lambda (e)
      (make-definition (make-variable 'dummy) e)))

  ;; Transformations

  (define transform
    (lambda (f x ribs)
      (apply-transformer f (apply-anti-mark x) ribs)))

  (define apply-transformer
    (lambda (f x ribs)
      (let ([f (cond
                [(variable-transformer? f)
                 (variable-transformer-proc f)]
                [(procedure? f) f]
                [else (assert #f)])])
        (guard (c
                [(invalid-syntax-object-condition? c)
                 (syntax-error #f (format "encountered invalid object ~s in output of macro"
                                    (invalid-syntax-object-irritant c)))])
          (wrap-syntax-object (f x) (make-mark) ribs)))))

  ;; syntax-type

  (define syntax-type
    (lambda (x ribs)
      (define keyword-type
        (lambda (bdg)
          (syntax-type
           (transform (cond
                       [(keyword-binding? bdg)
                        (let ([lib (keyword-binding-library bdg)])
                          (when lib (library-visit! lib)))
                        (assert (keyword-binding-transformer bdg))]
                       [else (assert #f)])
                      x
                      ribs)
           ribs)))
      (syntax-match x
        [(,k . ,x*)
         (guard ($identifier? k))
         (let* ([lbl (identifier->label k)]
                [bdg (label->binding lbl)])
           (cond
            [(or (splicing-binding? bdg)
                 (expander-binding? bdg)
                 (definition-binding? bdg)
                 (prim-binding? bdg))
             (values x bdg)]
            [(keyword-binding? bdg)
             (keyword-type bdg)]
            [else
             (values x (make-application-type))]))]
        [(,f . ,x*)
         (values x (make-application-type))]
        [,x
         (guard ($identifier? x))
         (cond
          [(identifier->label x)
           => (lambda (lbl)
                (let ([bdg (label->binding lbl)])
                  (cond
                   [(or (variable-binding? bdg))
                    (values x bdg)]
                   [(keyword-binding? bdg)
                    (keyword-type bdg)]
                   [(auxiliary-binding? bdg)
                    (syntax-error (auxiliary-binding-who bdg) "invalid use of auxiliary syntax" x)]
                   [(out-of-phase-binding? bdg)
                    (syntax-error #f "identifier referenced out of phase" x)]
                   [(displaced-binding? bdg)
                    (syntax-error #f "identifier referenced out of context" x)]
                   [else
                    (values x (make-other-type))])))]
          [else
           (undefined-error x "unbound identifier ~a")])]
        [,x
         (let ([e (syntax-object->datum x)])
           (unless (constant? e)
             (syntax-error #f "invalid expression syntax" x))
           (values x (make-constant-type e)))])))

  ;; Libraries

  (module (expand-library)
    ;; TODO: For programs/libs(?): we can have a true body at the end.
    (define expand-library
      (lambda (name ver exp* imp* body*)
        (let ([ribs (make-ribcage)]
              [rib (make-rib)])
          (ribcage-add-barrier! ribs rib '(()))
          (for-each
            (lambda (imp)
              (import-spec-import! imp rib))
            imp*)
          (with-requirements-collector
              (let*-values ([(viscmd* def* e lbl*)
                             (expand-internal body* ribs (expansion-mode library))]
                            [(var* lib* ref-lbl*) (current-runtime-globals)]
                            [(loc*)
                             (vector-map variable-binding-location
                                         (vector-map label->binding ref-lbl*))])
                (assert (not e))
                (let ([exports (make-rib)]
                      [setters (build-variable-setters lbl*)])
                  (define invoke-code
                    (build-invoke-code def* setters var* loc*))
                  (define viscode
                    (build-visit-code viscmd*))
                  (for-each
                    (lambda (exp)
                      (export-spec-export! exp exports ribs))
                    exp*)
                  (values
                    (make-library
                     ;; Name
                     name
                     ;; Version
                     ver
                     ;; Uid
                     (uid (last name))
                     ;; Imports
                     '#()               ;FIXME
                     ;; Exports
                     exports
                     ;; Visit requirements
                     (collected-visit-requirements)
                     ;; Invoke requirements
                     (collected-invoke-requirements)
                     ;; Visit commands
                     viscode
                     ;; Invoke definitions
                     invoke-code
                     ;; Bindings
                     lbl*)
                    lbl*)))))))

    (define build-variable-setters
      (lambda (lbl*)
        (fold-left
          (lambda (def* lbl)
            (let ([bdg (label->binding lbl)])
              (if (variable-binding? bdg)
                  (cons (build (location-box-set! ',(variable-binding-location bdg)
                                                  ,(variable-binding-symbol bdg)))
                        def*)
                  def*)))
          '() lbl*)))

    (define build-invoke-code
      (lambda (def* setter* vars locs)
        (build
          (letrec ,(map (lambda (var loc)
                          `[,var (location-box ',loc)])
                        (vector->list vars)
                        (vector->list locs))
            (letrec* ,(map (lambda (def)
                             `[,(definition-var def) ,(definition-expr def)])
                           def*)
              (begin
                ,@setter*
                (values)))))))

    (define build-visit-code
      (lambda (viscmd*)
        (build
          (begin
            ,@viscmd*
            (values)))))

    #;
    (define build-invoker
      (lambda (def* setters vars vals)
        (compile-to-thunk
         (build
           (letrec (,(map (lambda (var loc)
                            `[,var ',loc])
                          (vector->list vars)
                          (vector->list vals))
                    ...)
             (begin
               ,(map
                  (lambda (var)
                    `(set! ,var (location-box ,var)))
                  (vector->list vars))
               ...
               (letrec* (,(map (lambda (def)
                                 `[,(definition-var def)
                                   ,(definition-expr def)])
                               def*)
                         ...)
                 (begin
                   ,setters ...
                   (values))))))))))

  (define/who export-spec-export!
    (define parse-export-spec
      (lambda (x)
        (syntax-match x
	  [,id
	   (guard ($identifier? id))
	   (values (list id) (list id))]
	  [(rename (,orig* ,new*) ...)
	   (guard (for-all $identifier? orig*)
	          (for-all $identifier? new*))
	   (values orig* new*)]
	  [,x
	   (syntax-error #f "invalid export spec" #f x)])))
    (lambda (spec rib body-ribcage)
      (define doexport!
        (lambda (orig new)
          (let ([name (identifier->symbol new)]
                [marks (syntax-object-marks new)])
            (when (rib-ref rib name marks)
              (identifier-error #f new "identifier ~a exported twice"))
            (cond
             [(identifier->label/props (add-substitutions body-ribcage orig))
              => (lambda (l/p)
                   (rib-set! rib name marks l/p))]
             [else
              (identifier-error #f orig "attempt to export unbound identifier ~a")]))))
      ;; expand-spec-export!
      (let-values ([(orig* new*) (parse-export-spec spec)])
        (for-each
          (lambda (orig new)
            (doexport! orig new))
          orig* new*))))


  )
