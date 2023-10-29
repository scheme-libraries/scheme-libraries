#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs eval (6))
  (export
    eval
    environment)
  (import
    ($system)
    (scheme-libraries syntax library-loaders)
    (scheme-libraries syntax library-locators)
    (scheme-libraries syntax library-collections)
    (scheme-libraries syntax import-specs)
    (scheme-libraries syntax default-stdlibs-collections)
    (rename (scheme-libraries syntax eval)
      (environment $environment)))

  (define environment
    (lambda import-spec*
      (apply $environment import-spec*)))

  ;; XXX: Fix library expressions!
  (define bla
    (current-library-collection (make-default-stdlibs-collection)))

  (define blub
    ;; FIXME: Make configurable.
    (current-library-loader
     (make-default-library-loader (make-library-locator '("lib/") '(".sls"))))))
