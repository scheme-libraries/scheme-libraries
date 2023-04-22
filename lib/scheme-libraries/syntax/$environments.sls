#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $environments)
  (export
    make-environment
    environment?
    environment-rib
    environment-set!)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries record-writer)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax $ribs))

  ;; Environments

  (define-record-type environment
    (nongenerative environment-3cd8d34b-252d-4240-8950-326edbf47a4f)
    (sealed #t)
    (fields rib)
    (protocol
      (lambda (new)
        (define who 'make-environment)
        (case-lambda
          [(rib)
           (unless (rib? rib)
             (assertion-violation who "invalid rib argument" rib))
           (new rib)]
          [()
           (new (make-rib))]))))

  (define/who environment-set!
    (lambda (env name l/p)
      (unless (environment? env)
        (assertion-violation who "invalid environment argument" env))
      (unless (symbol? name)
        (assertion-violation who "invalid name argument" name))
      (unless (label/props? l/p)
        (assertion-violation who "invalid label/props argument" name))
      (rib-set! (environment-rib env) name '() l/p)))

  ;; Record writers

  (record-writer (record-type-descriptor environment)
    (lambda (r p wr)
      (put-string p "#<environment>")))

  )
