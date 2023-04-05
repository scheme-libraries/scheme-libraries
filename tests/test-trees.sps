#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries trees)
  (scheme-libraries testing))

(test-begin "trees")

(test-equal '() (tree->list '()))
(test-equal '(a) (tree->list 'a))
(test-equal '(a) (tree->list '(a)))
(test-equal '(a) (tree->list '(() . a)))
(test-equal '(a b c d e) (tree->list (cons '(a b c) '(d e))))
(test-equal '(a b c d e) (tree->list (cons 'a '(b c d e))))
(test-equal '(a b c d e) (tree->list (cons '(a b c d) 'e)))

(test-end "trees")
