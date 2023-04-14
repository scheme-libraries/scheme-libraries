#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax bootstrap-environment)
  (export
    bootstrap-environment)
  (import
    (rnrs)
    (scheme-libraries helpers)
    (scheme-libraries match)
    (scheme-libraries parameters)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax expand)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects)
    (scheme-libraries syntax variables)
    (scheme-libraries thread-parameters))

  (define-syntax declare-syntax
    (lambda (stx)
      (syntax-case stx ()
          [(_ name bdg)
           #`(let ([l/p (make-label/props (make-label bdg (metalevel:syntax) 'name))])
               (environment-set! (system-environment) 'name l/p))])))

  (define-syntax declare-expander-syntax
    (syntax-rules ()
      [(declare-expander-syntax name proc)
       (declare-syntax name (make-expander-binding proc))]))

  (define-syntax declare-splicing-syntax
    (syntax-rules ()
      [(declare-splicing-syntax name proc)
       (declare-syntax name (make-splicing-binding proc))]))

  (define-syntax declare-definition-syntax
    (syntax-rules ()
      [(declare-expander-syntax name proc)
       (declare-syntax name (make-definition-binding proc))]))

  (define-syntax declare-auxiliary-syntax
    (syntax-rules ()
      [(declare-expander-syntax name)
       (declare-syntax name (make-auxiliary-binding 'name))]))

  (define-syntax declare-prim-syntax
    (syntax-rules ()
      [(declare-expander-syntax name arity)
       (declare-syntax name (make-prim-binding 'name arity))]))

  ;; Helpers

  (define parse-define
    (lambda (x)
      (define who 'define)
      (syntax-match x
        [(,k ,x ,e)
         (guard ($identifier? x))
	 (values x e)]
        [(,k ,x)
         (guard ($identifier? x))
         (values x `(void))]
	[(,k (,x . ,formals) ,e)
	 (guard ($identifier? x))
	 (values x `(lambda ,formals ,e))]
        [,x
         (syntax-error who "invalid syntax" x)])))

  (define parse-define-syntax
    (lambda (x)
      (define who 'define-syntax)
      (syntax-match x
        [(,k ,x ,e)
         (guard ($identifier? x))
         (values x e)]
        [,x (syntax-error who "invalid syntax" x)])))

  (define parse-formals
    (lambda (who form x)
      (syntax-match x
        [(,x* ...)
         (guard (valid-bound-identifiers? x*))
         x*]
        [(,x* ... . ,x)
         (guard (valid-bound-identifiers? (cons x x*)))
         `(,x* ... . ,x)]
        [,x (syntax-error who "invalid syntax")])))

  (define formals-map
    (lambda (proc formals)
      (match formals
        [() '()]
        [(,x . ,[x*]) `(,(proc x) . ,x*)]
        [,x (proc x)])))

  (define formals->list
    (lambda (formals)
      (match formals
        [() '()]
        [(,x . ,[x*]) `(,x . ,x*)]
        [,x `(,x)])))

  (define valid-bound-identifiers?
    (lambda (stx*)
      ;; TODO: The algorithm is currently quadratic, which may matter
      ;; for generated code.
      (let f ([stx* stx*]
	      [id* '()])
	(or (null? stx*)
	    (let ([stx (car stx*)])
	      (and ($identifier? stx)
		   (not (find
			 (lambda (id)
			   ($bound-identifier=? id stx))
			 id*))
		   (f (cdr stx*) (cons stx id*))))))))

  (define expand-letrec
    (lambda (x who)
      (syntax-match x
        [(,k ([,x* ,e*] ...) ,b* ... ,b)
         (guard (for-all $identifier? x*))
         (unless (valid-bound-identifiers? x*)
           (syntax-error who "invalid syntax" x))
         (let* ([name* (map identifier->symbol x*)]
                [var* (map make-variable name*)]
                [bdg* (map make-variable-binding var*)]
                [lbl* (map make-label bdg*)]
                [ribs (make-ribcage x* lbl*)]
                [e* (add-substitutions* ribs e*)]
                [form* (add-substitutions* ribs `(,b* ... ,b))])
           (parameterize ([current-who who]
                          [current-form x])
             (let ([e* (map expand-expression e*)]
                   [e (expand-body form*)])
               (for-each label-kill! lbl*)
               (build (,who ([,var* ,e*] ...) ,e)))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (define expand-let-syntax
    (lambda (x who)
      (syntax-match x
        [(,k ([,x* ,e*] ...) ,b* ...)
         (guard (for-all $identifier? x*))
         (let* ([lbl* (map
                        (lambda (x)
                          (make-label #f (current-metalevel-for-syntax)))
                        x*)]
                [ribs (make-ribcage x* lbl*)]
                [bdg*
                 (map
                   (lambda (x lbl e)
                     (let* ([e (case who
                                 [(let-syntax) e]
                                 [(letrec-syntax)
                                  (add-substitutions ribs e)]
                                 [else #f])]
                            [proc (eval-transformer e who)])
                       (make-keyword-binding proc)))
                   x* lbl* e*)])
           (for-each label-bind! lbl* bdg*)
           (add-substitutions* ribs b*))]
        [,x (syntax-error who "invalid syntax" x)])))

  ;; Macros

  (define eval-transformer
    (lambda (x who)
      (let ([e (meta-expand x)])
        (let ([f (execute-transformer e)])
          ;; TODO: variable transformer
          (unless (procedure? f)
            (assertion-violation who "invalid transformer" f))
          f))))

  (define meta-expand
    (lambda (x)
      ;; TODO: Switch context and collect information about visited
      ;; symbols.
      (let ([e (parameterize ([current-metalevel
                               (fx+ (current-metalevel) 1)])
                 (expand-expression x))])
        e)))

  (define execute-transformer
    (lambda (e)
      ;; TODO Wrap e with code so that the runtime globals coming from
      ;; the expansion of e (in the meta-context) are set.  (The
      ;; values are also delivered in a vector.)
      (execute e)))

  (define execute
    (lambda (e)
      ((compile-to-thunk e))))

  ;; Syntax-case

  (define syntax-case-expander
    (lambda (x)
      ;; FIXME
      (assert #f)))

  (define syntax-expander
    (lambda (depth)
      (lambda (x)
        (define gen-template
          (lambda (tmpl depth)
            (let f ([tmpl tmpl] [depth depth] [env* '()] [tail? #f])
              (syntax-match tmpl
                ;; <identifier>
                [,tmpl
                 (guard ($identifier? tmpl))
                 (cond
                  ;; FIXME: Handle pattern variables
                  [else
                   (values `($syntax ,tmpl) env* #f)])]
                ;; <constant>
                [,tmpl
                 (values
                   (if (and tail? (syntax-null? tmpl))
                       `(quote ())
                       `($syntax ,tmpl))
                   env*
                   #f)]))))
        (syntax-match x
          [(,k ,tmpl)
           (let-values ([(out env* var?)
                         (gen-template tmpl depth)])
             (expand-expression out))]
          [,x
           (syntax-error #f "invalid syntax" x)]))))

  ;; Bootstrap environment

  (define bootstrap-environment
    (lambda ()
      ;; TODO: Copy the system environment.
      (system-environment)))

  ;; Syntax

  ;; Auxiliary syntax

  (declare-auxiliary-syntax =>)
  (declare-auxiliary-syntax else)

  ;; Definitions

  (declare-definition-syntax define
    (lambda (x ribs)
      (let-values ([(x e) (parse-define x)])
        (let* ([var (make-variable (identifier->symbol x))]
               [bdg (make-variable-binding var)]
               [lbl (ribcage-add! ribs x bdg)])
          (unless lbl
            (identifier-error 'define x "trying to redefine the local keyword ~a"))
          (list (lambda ()
                  (make-definition var (expand-expression e))))))))

  (declare-definition-syntax define-syntax
    (lambda (x ribs)
      (define who 'define-syntax)
      (let-values ([(x e) (parse-define-syntax x)])
        (let* ([proc (eval-transformer e who)]
               [bdg (make-keyword-binding proc)]
               [lbl (ribcage-add! ribs x bdg (current-metalevel-for-syntax))])
          (unless lbl
            (identifier-error 'define-syntax x "trying to redefine the local keyword ~a"))
          '()))))

  ;; Splicing syntax

  (declare-splicing-syntax begin
    (lambda (x)
      (define who 'begin)
      (syntax-match x
        [(,k ,x* ...)
         x*]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-splicing-syntax let-syntax
    (lambda (x)
      (expand-let-syntax x 'let-syntax)))

  (declare-splicing-syntax letrec-syntax
    (lambda (x)
      (expand-let-syntax x 'letrec-syntax)))

  ;; Expanders

  (declare-expander-syntax lambda
    (lambda (x)
      (define who 'lambda)
      (syntax-match x
        [(,k ,formals ,body* ... ,body)
         (let* ([formals (parse-formals who x formals)]
                [names (formals-map identifier->symbol formals)]
                [vars (formals-map make-variable names)]
                [id* (formals->list formals)]
                [bdg* (map make-variable-binding (formals->list vars))]
                [lbl* (map make-label bdg*)]
                [ribs (make-ribcage id* lbl*)]
                [form* (add-substitutions* ribs `(,body* ... ,body))]
                [e (parameterize ([current-who who]
                                  [current-form x])
                     (expand-body form*))])
           (for-each label-kill! lbl*)
           (build
             (lambda ,vars ,e)))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax if
    (lambda (x)
      (define who 'if)
      (syntax-match x
        [(,k ,[expand-expression -> e0] ,[expand-expression -> e1] ,[expand-expression -> e2])
         (build (if ,e0 ,e1 ,e2))]
        [(,k ,[expand-expression -> e0] ,[expand-expression -> e1])
         (build (if ,e0 ,e1 (values)))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax quote
    (lambda (x)
      (define who 'quote)
      (syntax-match x
        [(,k ,d)
         (build (quote ,(syntax-object->datum d)))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax set!
    (lambda (x)
      (define who 'set!)
      (syntax-match x
        [(,k ,x ,[expand-expression -> e])
         (guard ($identifier? x))
         ;; TODO: Variable transformers.
         `(set! ,x ,e)]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax and
    (lambda (x)
      (define who 'and)
      (syntax-match x
        [(,k) (build (quote #t))]
        [(,k ,[expand-expression -> e] ,[expand-expression -> e*] ...)
         (let f ([e e] [e* e*])
           (if (null? e*)
               e
               (build `(if ,e ,(f (car e*) (cdr e*)) '#f))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax or
    (lambda (x)
      (define who 'or)
      (syntax-match x
        [(,k) (build (quote #f))]
        [(,k ,[expand-expression -> e] ,[expand-expression -> e*] ...)
         (let f ([e e] [e* e*])
           (if (null? e*)
               e
               (let ([t (make-variable 't)])
                 (build-let ([,t ,e]) (if ,t ,t ,(f (car e*) (cdr e*)))))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax let
    (lambda (x)
      (define who 'let)
      (syntax-match x
        [(,k ([,x* ,e*] ...) ,b* ... ,b)
         (guard (for-all $identifier? x*))
         (expand-expression `((lambda ,x* ,b* ... ,b) ,e* ...))]
        [(,k ,f ([,x* ,e*] ...) ,b* ... ,b)
         (guard ($identifier? f) (for-all $identifier? x*))
         (expand-expression `(letrec ([,f (lambda ,x* ,b* ... ,b)]) (,f ,e* ...)))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax letrec
    (lambda (x)
      (expand-letrec x 'letrec)))

  (declare-expander-syntax letrec*
    (lambda (x)
      (expand-letrec x 'letrec*)))

  (declare-expander-syntax let*
    (lambda (x)
      (define who 'let*)
      (syntax-match x
        [(,k () ,b* ... ,b)
         (expand-body `(,b* ... ,b))]
        [(,k ([,x ,e] [,x* ,e*] ...) ,b* ... ,b)
         (guard (for-all $identifier? x*))
         (let f ([x x] [x* x*] [e e] [e* e*])
           (if (null? x*)
               (expand `(let ([,x ,e]) ,b* ... ,b))
               (expand `(let ([,x ,e])
                          ,(f (car x*) (cdr x*)
                              (car e*) (cdr e*))))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax cond
    (lambda (x)
      (define who 'cond)
      (syntax-match x
        [(,k ,cl ,cl* ...)
         (let f ([cl cl] [cl* cl*])
           (if (null? cl*)
               (syntax-match cl
                 [(else ,[expand-expression -> e] ,[expand-expression -> e*] ...)
                  (build-begin ,e ,e* ...)]
                 [(,[expand-expression -> t] => ,[expand-expression -> e])
                  (let ([x (make-variable 't)])
                    (build-let ([,x ,t])
                      (if ,x (,e ,x) (values))))]
                 [(,[expand-expression -> t])
                  (let ([x (make-variable 't)])
                    (build-let ([,x ,t])
                      (if ,x ,x (values))))]
                 [(,[expand-expression -> t] ,[expand-expression -> e*] ...)
                  (build (if ,t ,(build-begin ,e* ...) (values)))]
                 [,cl (syntax-error who "invalid clause" x cl)])
               (let ([rest (f (car cl*) (cdr cl*))])
                 (syntax-match cl
                   [(,[expand-expression -> t] => ,[expand-expression -> e])
                    (let ([x (make-variable 't)])
                      (build-let ([,x ,t])
                        (if ,x (,e ,x) ,rest)))]
                   [(,[expand-expression -> t])
                    (let ([x (make-variable 't)])
                      (build-let ([,x ,t])
                        (if ,x ,x ,rest)))]
                   [(,[expand-expression -> t] ,[expand-expression -> e*] ...)
                    (build (if ,t ,(build-begin ,e* ...) ,rest))]
                   [,cl (syntax-error who "invalid clause" x cl)]))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax case
    (lambda (x)
      (define who 'case)
      (syntax-match x
        [(,k ,e ,cl ,cl* ...)
         (expand-expression
          `(let ([k ,e])
             (cond
              ,(let f ([cl cl] [cl* cl*])
                 (if (null? cl*)
                     (syntax-match cl
                       [[else ,e ,e* ...]
                        `[else ,e ,e* ...]]
                       [[(,d ...) ,e ,e* ...]
                        `[(memv k '(,d ...)) ,e ,e* ...]]
                       [,cl (syntax-error who "invalid clause" x cl)])
                    (let ([rest (f (car cl*) (cdr cl*))])
                      (syntax-match cl
                        [[(,d ...) ,e ,e* ...]
                         `[(memv k '(,d ...)) ,e ,e* ...]]
                        [,cl (syntax-error who "invalid clause" x cl)])))))))]
        [,x (syntax-error who "invalid sytnax" x)])))

  (declare-expander-syntax syntax-case
    syntax-case-expander)

  (declare-expander-syntax syntax
    (syntax-expander #f))

  (declare-expander-syntax $syntax
    (lambda (x)
      (define who '$syntax)
      (syntax-match x
        [(,k ,e) (build `(quote ,e))]
        [,x (syntax-error who "invalid syntax" x)])))

  ;; prims

  (declare-prim-syntax void 0)
  (declare-prim-syntax memv 2)

  )
