#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries languages)
        (scheme-libraries testing))

(define-language Lsrc
  ()
  )

(test-begin "languages")

(test-end "languages")
