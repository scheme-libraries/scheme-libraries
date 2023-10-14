#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax default-stdlibs-collections)
  (export
    make-default-stdlibs-collection)
  (import
    (rnrs)
    (scheme-libraries syntax library-locators)
    (scheme-libraries syntax stdlibs-collections))

  (define make-default-stdlibs-collection
    (lambda ()
      (default-stdlibs-collection)))

  (define-syntax default-stdlibs-collection
    (lambda (stx)
      #'(stdlibs-collection (make-library-locator '("stdlib/") '(".sls"))
                            ((rnrs base) #t)
                            ((rnrs) #t)))))
