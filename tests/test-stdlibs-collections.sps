#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries syntax $serializing)
        (scheme-libraries syntax stdlibs-collection-datums)
        (scheme-libraries syntax library-collections)
        (scheme-libraries syntax library-locators)
        (scheme-libraries syntax eval)
        (scheme-libraries testing))

(define stdlibs
  (datum->library-collection
   (stdlibs-collection-datum
    (make-library-locator '("stdlib/" "tests/lib/") '(".sls"))
    ((rnrs base) #t)
    ((rnrs) #t)
    ((test) #t))))

(test-begin "standard libraries collections")

(test-assert (library-collection? stdlibs))

(current-library-collection stdlibs)

(test-equal 4 (eval '4 (environment '(rnrs))))

(test-equal 12 (eval '(foo) (environment '(test))))

(test-assert (eval '(record-type-descriptor? (record-type-descriptor bar))
                   (environment '(test) '(rnrs))))

(test-end)
