#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries syntax stdlibs-collections)
        (scheme-libraries syntax library-collections)
        (scheme-libraries syntax library-locators)
        (scheme-libraries testing))

(define stdlibs
  (stdlibs-collection
   (make-library-locator '() '())
   ((rnrs) #t #t)))

(test-begin "standard libraries collections")

(test-assert (library-collection? stdlibs))

(test-end)
