#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries vectors))

(test-begin "vectors")

(test-equal '(2 c z (1 b y (0 a x ())))
  (vector-fold-right list '() '#(a b c) '#(x y z)))

(test-end "vectors")
