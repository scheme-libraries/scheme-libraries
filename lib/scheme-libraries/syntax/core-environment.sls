#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax core-environment)
  (export
    core-environment)
  (import
    (rnrs)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects)
    (scheme-libraries syntax variables))

  ;; Core environment

  (define core-environment
    (make-environment (make-rib)))

  (define expand-lambda
    (lambda (x)
      (syntax-match x
        [(,k ,formals ,body* ... ,body*)
         (let* ([formals (parse-formals formals)]
                [names (formals-map identifier->symbol formals)]
                [vars (formals-map make-variable names)]
                [id* formals->list formals]
                [bdg* (map make-variable-binding (formals->list vars))]
                ;; Check whether we need the current metalevel here.
                [lbl* (map make-label bdg*)]
                [ribs (make-ribcage id* lbl*)]
                ;; TODO: we should put the substitutions on top of the
                ;; ribs outselves!  TODO: check where
                ;; add-substitutions* is used... maybe we don't have
                ;; to expose make-ribcage
                [e (expand-body `(,body* ... ,body) ribs)])
           (for-each label-kill! lbl*)
           `(lambda ,vars ,e))]
        [,x (syntax-error 'lambda "invalid syntax" x)])))

  (define parse-formals
    (lambda (who form x)
      (syntax-match x
        [(,x* ...)
         (guard (valid-bound-identifier? x*))
         x*]
        [(,x* ... . x)
         (guard (valid-bound-identifier? (cons x x*)))
         `(,x* ... . x)]
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



  )
