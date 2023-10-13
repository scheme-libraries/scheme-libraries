#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries syntax stdlibs-collections)
        (scheme-libraries syntax library-collections)
        (scheme-libraries syntax library-locators)
        (scheme-libraries syntax eval)
        (scheme-libraries testing))

(define stdlibs
  (stdlibs-collection
   (make-library-locator '("stdlib/" "tests/") '(".sls"))
   ((rnrs base) #t #t)
   ((rnrs) #t #t)
   ((test) #t #t)))

(test-begin "standard libraries collections")

(test-assert (library-collection? stdlibs))

(current-library-collection stdlibs)

(test-equal 4 (eval '4 (environment '(rnrs base))))

(eval 'foo (environment '(test)))

(test-equal 12 (eval 'foo (environment '(test))))

(test-end)
