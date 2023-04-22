#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax eval)
  (export
    eval
    eval-annotated-datum
    environment)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries syntax $environments)
    (scheme-libraries syntax $ribs)
    (scheme-libraries syntax expand)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax import-specs)
    (scheme-libraries syntax syntax-match)
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

  (define/who environment
    (lambda imp-spec*
      (let ([rib (make-rib)])
        (for-each
          (lambda (imp-spec)
            (import-spec-import! (annotated-datum->syntax-object
                                  (datum->annotated-datum imp-spec)
                                  (system-environment))
                                 rib))
          imp-spec*)
        (make-environment rib)))))
