#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries boxes))

(test-begin "boxes")

(test-assert (box? (box 3)))
(test-eqv 4 (unbox (box 4)))

(test-eqv 5
  (let ([b (box 2)])
    (set-box! b 5)
    (unbox b)))

(test-end "boxes")
