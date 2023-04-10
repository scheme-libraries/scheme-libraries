#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries reading annotated-datums)
  (scheme-libraries syntax syntax-objects))

(test-begin "syntax objects")

(define tmpl (annotated-datum->syntax-object (datum->annotated-datum 'tmpl)))

(test-assert (syntax-object? tmpl))
(test-equal 'tmpl (syntax-object->datum tmpl))

(test-equal '(a b) (syntax-object->datum (datum->syntax-object tmpl '(a b))))

(test-equal 'a (syntax-object->datum (syntax-car (datum->syntax-object tmpl '(a b)))))

(test-end "syntax objects")
