#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax variables)
  (export
    make-variable
    variable?)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries gensyms))

  (define/who make-variable
    (lambda (name)
      (unless (symbol? name)
        (assertion-violation "invalid name argument" name))
      (gensym (symbol->string name))))

  (define variable?
    (lambda (x)
      (symbol? x))))
