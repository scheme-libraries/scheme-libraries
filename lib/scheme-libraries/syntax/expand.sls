#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expand)
  (export
    expand)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries record-writers)
    (scheme-libraries syntax syntax-objects))

  (define-record-type (environment make-environment $environment)
    (nongenerative environment-3cd8d34b-252d-4240-8950-326edbf47a4f)
    (sealed #t)
    (fields rib))

  (define/who expand
    (lambda (expr env)
      (unless (annotated-datum? expr)
        (assertion-violation who "invalid expression argument" expr))
      (unless ($environment? env)
        (assertion-violation who "invalid environment argument" env))


      ;; FIXME
      (assert #f)))

  ;; Record writers

  (record-writer (record-type-descriptor environment)
    (lambda (r p wr)
      (put-string p "#<environment>")))
  )
