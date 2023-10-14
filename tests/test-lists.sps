#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries lists)
  (scheme-libraries testing))

(test-begin "lists")

(test-equal '3 (last '(1 2 3)))
(test-equal '1 (last '(1)))

(test-equal '(4 8 12)
  (filter-map (lambda (x)
                (and (even? x) (* 2 x)))
              '(1 2 3 4 5 6)))

(test-equal '(0 1 2)
  (take (iota 5) 3))

(test-end "lists")
