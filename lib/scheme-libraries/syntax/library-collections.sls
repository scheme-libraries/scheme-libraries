#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax library-collections)
  (export
    current-library-collection
    bootstrap-library-collection)
  (import
    (rnrs)
    (scheme-libraries libraries)
    (scheme-libraries syntax-match)
    (scheme-libraries thread-parameters)
    (scheme-libraries define-who))

  ;; Library collections

  (define-record-type library-collection
    (nongenerative library-collection-765810f4-88f2-47a3-9d4a-df90941f0a82)
    (sealed #t)
    (fields libraries)
    (protocol
      (lambda (new)
        (new (make-library-table)))))

  ;; Current library collection

  (define/who current-library-collection
    (make-thread-parameter (make-library-collection)
      (lambda (x)
        (unless (library-collection? x)
          (assertion-violation who "invalid library collection" x))
        x)))

  (define current-library-table
    (lambda ()
      (library-collection-libaries (current-library-collection))))

  (define library-set!
    (lambda (name lib)
      (library-table-set! (current-library-table) name lib)))

  ;; TODO: locate library for high-level environment procedure.

  ;; Bootstrap library collection

  (define bootstrap-library-collection
    (lambda ()
      (let ([lc (make-library-collection)])
        (parameterize ([current-library-collection lc])
          (library-set! '($system) (make-system-library))
          lc))))

  (define make-system-library
    (lambda ()
      (make-library '($system) '() (environment-rib (system-environment)))))

  )
