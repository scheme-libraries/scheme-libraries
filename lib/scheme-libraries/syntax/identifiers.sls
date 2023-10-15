#!r6rs

;;; Copyright © 2023 Marc Nieper-Wißkirchen

(library (scheme-libraries syntax identifiers)
  (export
    $generate-temporaries)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries syntax syntax-objects)
    (scheme-libraries syntax syntax-match))

  (define/who $generate-temporaries
    (lambda (x)
      (syntax-match x
        [(,x* ...)
         (map (lambda (x) (generate-temporary)) x*)]
        [,x (assertion-violation who "invalid list argument" x)]))))
