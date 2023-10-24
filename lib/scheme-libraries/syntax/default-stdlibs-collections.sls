#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax default-stdlibs-collections)
  (export
    make-default-stdlibs-collection)
  (import
    (rnrs)
    (scheme-libraries ports)
    (scheme-libraries syntax library-locators)
    (scheme-libraries syntax stdlibs-collections))

  (define make-default-stdlibs-collection
    (lambda ()
      (default-stdlibs-collection)))

  (define-syntax default-stdlibs-collection
    (lambda (stx)
      (syntax-case stx ()
        [(k)
         (with-syntax ([(libspec ...) (read-file "config/stdlibs.scm" #'k)])
           #'(stdlibs-collection (make-library-locator '("stdlib/" "lib/") '(".sls"))
                                 (libspec #t) ...))]))))
