#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries reading annotated-datums)
  (scheme-libraries syntax syntax-objects))

(define tmpl (annotated-datum->syntax-object (datum->annotated-datum 'tmpl)))

(define-syntax $syntax
  (syntax-rules ()
    [($syntax datum)
     (datum->syntax-object tmpl 'datum)]))

(test-begin "syntax objects")

(test-assert (syntax-object? ($syntax foo)))
(test-equal 'foo (syntax-object->datum ($syntax foo)))

(test-equal '(a b) (syntax-object->datum ($syntax (a b))))

(test-equal 'a (syntax-object->datum (syntax-car ($syntax (a b)))))

(test-end "syntax objects")
