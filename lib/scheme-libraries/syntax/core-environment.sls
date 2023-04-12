#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax core-environment)
  (export
    core-environment)
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

  ;; Core environment

  (define core-environment
    (let ([env (make-environment)])
      (lambda () env)))

  (define-syntax declare-syntax
    (lambda (stx)
      (syntax-case stx ()
          [(_ name bdg)
           #`(define #,(construct-name #'name "$" #'name)
               (let ([l/p (make-label/props (make-label bdg (metalevel:syntax)))])
                 (environment-set! (core-environment) 'name l/p)
                 (annotated-datum->syntax-object (make-annotated-atom 'name #f)
                                                 (core-environment))))])))

  (define-syntax declare-expander-syntax
    (syntax-rules ()
      [(declare-expander-syntax name proc)
       (declare-syntax name (make-expander-binding proc))]))

  (define-syntax declare-definition-syntax
    (syntax-rules ()
      [(declare-expander-syntax name proc)
       (declare-syntax name (make-definition-binding proc))]))

  (define-syntax declare-prim-syntax
    (syntax-rules ()
      [(declare-expander-syntax name arity)
       (declare-syntax name (make-prim-binding 'name arity))]))

  ;; Definitions

  (declare-definition-syntax define
    (lambda (x ribs)
      (let-values ([(x e) (parse-define x)])
        (let* ([var (make-variable (identifier->symbol x))]
               [bdg (make-variable-binding var)]
               [lbl (ribcage-add! ribs x bdg)])
          (unless lbl
            (identifier-error 'define x "trying to redefine the local keyword ~a"))
          (values (list (lambda ()
                          (make-definition var (expand-expression e)))))))))

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

  ;; Prims

  (declare-prim-syntax void 0)

  ;; Helpers

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



  )
