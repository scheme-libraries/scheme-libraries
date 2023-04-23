#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax eval)
  (export
    eval
    eval-annotated-datum
    environment)
  (import
    (rnrs)
    (scheme-libraries boxes)
    (scheme-libraries define-who)
    (scheme-libraries parameters)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries syntax $environments)
    (scheme-libraries syntax $ribs)
    (scheme-libraries syntax expand)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax import-specs)
    (scheme-libraries syntax libraries)
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
      (parameterize ([current-requirements-collector (make-requirements-collector)])
        (let ([e (expand-expression (annotated-datum->syntax-object expr env))])
          (vector-for-each library-invoke! (collected-invoke-requirements))
          (let-values ([(vars libs locs) (current-runtime-globals)])
            ((compile-to-thunk
              (build
                (letrec ,(map (lambda (var loc)
                                `[,var ',(unbox loc)])
                              (vector->list vars)
                              (vector->list locs))
                  ,e)))))))))

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
