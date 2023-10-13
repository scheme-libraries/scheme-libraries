#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax variables)
  (export
    make-variable
    variable?
    variable-name
    name->variable)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries uuid)
    (scheme-libraries record-writer))

  (define-record-type (variable name->variable variable?)
    (nongenerative variable-f744dde0-a2f4-491a-b967-3b4bad286536)
    (fields name)
    (protocol
      (lambda (new)
        (define who 'name->variable)
        (lambda (name)
          (unless (symbol? name)
            (assertion-violation who "invalid name argument" name))
          (new name)))))

  (define/who make-variable
    (lambda (name)
      (unless (symbol? name)
        (assertion-violation who "invalid name argument" name))
      (name->variable (uid name))))

  ;; Record writer

  (record-writer (record-type-descriptor variable)
    (lambda (r p wr)
      (put-string p "#<variable ")
      (put-string p (symbol->string (variable-name r)))
      (put-string p ">")))

  )
