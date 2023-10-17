#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries quote-syntax)
  (scheme-libraries testing))

(test-begin "quote-syntax")

(test-assert (bound-identifier=? #'a (quote-syntax a)))

(test-assert (bound-identifier=? #'a (with-syntax ([a #'foo])
                                       (quote-syntax a))))

(test-end "quote-syntax")
