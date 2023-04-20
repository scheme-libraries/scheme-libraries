#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax library-collections)
  (export
    current-library-collection
    bootstrap-library-collection
    library-set!
    library-ref
    library-pending?
    library-pending!)
  (import
    (rnrs)
    (scheme-libraries parameters)
    (scheme-libraries thread-parameters)
    (scheme-libraries define-who)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects))

  ;; Library collections

  (define-record-type library-collection
    (nongenerative library-collection-765810f4-88f2-47a3-9d4a-df90941f0a82)
    (sealed #t)
    (fields libraries pending)
    (protocol
      (lambda (new)
        (lambda ()
          (new (make-library-table)
               (make-library-table))))))

  ;; Current library collection

  (define/who current-library-collection
    (make-thread-parameter (make-library-collection)
      (lambda (x)
        (unless (library-collection? x)
          (assertion-violation who "invalid library collection" x))
        x)))

  (define current-library-table
    (lambda ()
      (library-collection-libraries (current-library-collection))))

  (define current-pending-table
    (lambda ()
      (library-collection-pending (current-library-collection))))

  (define library-set!
    (lambda (name lib)
      (library-table-set! (current-library-table) name lib)))

  (define library-ref
    (lambda (name default)
      (library-table-ref (current-library-table) name default)))

  (define library-pending?
    (lambda (name)
      (library-table-contains? (current-pending-table) name)))

  (define library-pending!
    (lambda (name flag)
      (let ([tbl (current-pending-table)])
        (if flag
            (library-table-set! tbl name #t)
            (library-table-delete! tbl name)))))

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
