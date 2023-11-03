#!/usr/bin/env scheme-script
#!r6rs

(import (rnrs)
        (scheme-libraries parameters)
        (scheme-libraries syntax default-stdlibs-collections)
        (scheme-libraries syntax library-locators)
        (scheme-libraries syntax library-loaders)
        (scheme-libraries syntax library-collections)
        (scheme-libraries syntax import-specs)
        (scheme-libraries syntax current-command-line)
        (scheme-libraries syntax programs))

(current-library-collection (make-default-stdlibs-collection))
(current-library-loader (make-default-library-loader (make-library-locator '("lib/" "tests/lib/") '(".sls"))))

;;; FIXME: Test and guard against errors
(define filename (cadr (command-line)))

(parameterize ([current-command-line (cdr (command-line))])
  (load-program filename))

;; Local Variables:
;; mode: scheme
;; End:
