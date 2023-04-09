#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries lists)
  (scheme-libraries testing))

(test-begin "lists")

(test-equal '(4 8 12)
  (filter-map (lambda (x)
                (and (even? x) (* 2 x)))
              '(1 2 3 4 5 6)))

(test-end "lists")
