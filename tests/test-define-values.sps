#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries define-values)
        (scheme-libraries testing))

(test-begin "define-values")

(test-equal 1
  (let ()
    (define-values (a) 1)
    a))

(test-equal '(2 3)
  (let ()
    (define-values b (values 2 3))
    b))

(test-equal '(4 5)
  (let ()
    (define-values (x y) (values 4 5))
    (list x y)))

(test-equal '(6 (7))
  (let ()
    (define-values (u . v) (values 6 7))
    (list u v)))

(test-equal '(8 9)
  (let ()
    (define-values (u . v) (values 6 7))
    (set! u 8)
    (set! v 9)
    (list u v)))

(test-assert
    (let ()
      (define-values () (values))
      #t))

(test-end "define-values")
