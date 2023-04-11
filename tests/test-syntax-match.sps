#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries reading annotated-datums)
  (scheme-libraries syntax syntax-objects)
  (scheme-libraries syntax syntax-match))

(test-begin "syntax-match")

(test-assert 'dummy-test)

(test-end "syntax-match")
