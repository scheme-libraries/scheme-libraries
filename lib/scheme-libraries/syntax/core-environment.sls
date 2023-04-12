#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax core-environment)
  (export
    core-environment)
  (import
    (rnrs)
    (scheme-libraries match)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax expand)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects)
    (scheme-libraries syntax variables))

  ;; Expanders

  (define expand-lambda
    (lambda (x)
      (syntax-match x
        [(,k ,formals ,body* ... ,body)
         (let* ([formals (parse-formals formals)]
                [names (formals-map identifier->symbol formals)]
                [vars (formals-map make-variable names)]
                [id* (formals->list formals)]
                [bdg* (map make-variable-binding (formals->list vars))]
                [lbl* (map make-label bdg*)]
                [ribs (make-ribcage id* lbl*)]
                [form* (add-substitutions* `(,body* ... ,body))]
                [e (expand-body form*)])
           (for-each label-kill! lbl*)
           (extend-backquote here
             `(lambda ,vars ,e)))]
        [,x (syntax-error 'lambda "invalid syntax" x)])))

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

  ;; Core environment

  (define core-environment
    (make-environment))

  ;; TODO: Add bindings like an expander for expand-lambda.

  )
