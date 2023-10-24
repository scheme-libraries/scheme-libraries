#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries syntax bootstrap-environment)
  (scheme-libraries syntax library-collections)
  (scheme-libraries syntax default-stdlibs-collections)
  (scheme-libraries syntax eval)
  (scheme-libraries syntax import-specs)
  (scheme-libraries syntax library-loaders)
  (scheme-libraries syntax library-locators)
  (scheme-libraries syntax syntax-objects))

(current-library-collection (make-default-stdlibs-collection))

(define library-locator (make-library-locator '("tests/") '(".sls")))
(current-library-loader (make-default-library-loader library-locator))

(test-begin "eval")

(test-equal 4 (eval '4 (bootstrap-environment)))

(test-equal '(1 2 3) (eval '`(1 2 ,3) (bootstrap-environment)))
(test-equal '(1 2 3) (eval '`(1 2 ,'3) (bootstrap-environment)))

(test-equal 1 (eval 1 (environment '(rnrs base))))

(test-equal 12 (eval 'foo (environment '(test))))

(test-end "eval")
