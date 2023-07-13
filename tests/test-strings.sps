#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries strings)
  (scheme-libraries testing))

(test-begin "strings")

(test-equal '("a")
  (string-split "a"))

(test-equal '("a")
  (string-split " a "))

(test-equal '("a" "b")
  (string-split "a   b"))

(test-equal '()
  (string-split ""))

(test-equal '()
  (string-split "    "))

(test-equal '("ax" "by")
  (string-split " ax\nby\n"))

(test-end "strings")
