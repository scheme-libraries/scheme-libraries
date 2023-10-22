#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries filenames)
  (export
    filename?
    filename->s-exp
    s-exp->filename)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define filename?
    (lambda (obj)
      (string? obj)))

  (define/who filename->s-exp
    (lambda (filename)
      (unless (filename? filename)
        (assertion-violation who "invalid filename argument" filename))
      (string->symbol filename)))

  (define/who s-exp->filename
    (lambda (e)
      (unless (symbol? e)
        (assertion-violation who "invalid filename datum argument" e))
      (symbol->string e))))
