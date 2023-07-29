#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax variables)
  (export
    make-variable
    variable?)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries uuid))

  (define/who make-variable
    (lambda (name)
      (unless (symbol? name)
        (assertion-violation "invalid name argument" name))
      (uid name)))

  (define variable?
    (lambda (x)
      (symbol? x))))
