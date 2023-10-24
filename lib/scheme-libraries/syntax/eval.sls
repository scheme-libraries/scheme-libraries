#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax eval)
  (export
    eval
    eval-annotated-datum)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries syntax expand)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax syntax-objects))

  (define eval
    (lambda (expr env)
      (eval-annotated-datum (datum->annotated-datum expr) env)))

  (define/who eval-annotated-datum
    (lambda (expr env)
      (unless (annotated-datum? expr)
        (assertion-violation who "invalid expression argument" expr))
      (unless (environment? env)
        (assertion-violation who "invalid environment argument" env))
      ;; FIXME: Invoke the libraries referenced by the expression.
      ((compile-to-thunk (expand-expression (annotated-datum->syntax-object expr env))))))

  )
