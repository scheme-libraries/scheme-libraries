#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries multiple-values))

(test-begin "multiple values")

(test-equal '(-1 -2 -3)
  (let ()
    (define-values-map (x) - '(1 2 3))
    x))

(test-equal '((1 2 3) (5 7 9))
  (let ()
    (define-values-map (x y)
      (lambda (a b)
        (values a (+ a b)))
      '(1 2 3) '(4 5 6))
    (list x y)))

(test-end "multiple values")
