#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries syntax programs)
  (scheme-libraries syntax bootstrap-environment)
  (scheme-libraries syntax library-collections)
  (scheme-libraries syntax default-stdlibs-collections)
  (scheme-libraries syntax import-specs)
  (scheme-libraries syntax library-loaders)
  (scheme-libraries syntax library-locators)
  (scheme-libraries syntax syntax-objects))

(define library-locator (make-library-locator '("tests/") '(".sls")))
(current-library-collection (make-default-stdlibs-collection))
(current-library-loader (make-default-library-loader library-locator))

(test-begin "programs")

(test-equal '()
  (call-with-values (lambda () (load-program "tests/program1.sps"))
    list))

(test-equal '()
  (call-with-values (lambda () (load-program "tests/program2.sps"))
    list))


(test-end "programs")
