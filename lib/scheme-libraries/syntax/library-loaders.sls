#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax library-loaders)
  (export
    current-library-loader
    load-library)
  (import
    (rnrs)
    (scheme-libraries parameters)
    (scheme-libraries thread-parameters)
    (scheme-libraries define-who)
    (scheme-libraries syntax library-locators))

  ;; Library loaders

  (define-record-type library-loader
    (nongenerative library-loader-2c1d7725-3c34-49ab-858c-d9606bff95ae)
    (sealed #t)
    (fields locator)
    (protocol
      (lambda (new)
        (define who 'make-library-loader)
        (lambda (locator)
          (unless (library-locator? locator)
            (assertion-violation who "invalid locator argument" locator))
          (new locator)))))







  )
