#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax libraries)
  (export
    make-definition
    definition?
    definition-var
    definition-expr)
  (import
    (rnrs)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax variables))

  (define-record-type definition
    (nongenerative definition-02838b3f-218d-4416-b2e6-18058fe4815b)
    (fields var expr)
    (sealed #t)
    (protocol
     (lambda (new)
       (define who 'make-definition)
       (lambda (var expr)
         (unless (variable? var)
           (assertion-violation who "invalid variable argument" var))
         (unless (expression? expr)
           (assertion-violation who "invalid expression argument" expr))
         (new var expr)))))


  )
