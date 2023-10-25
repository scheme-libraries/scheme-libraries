#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $make-stdlibs-collection-datum)
  (export
    make-stdlibs-collection-datum
    make-stdlib
    stdlib?
    stdlib-name
    stdlib-pred
    stdlib-system?)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries helpers)
    (scheme-libraries parameters)
    (scheme-libraries syntax bootstrap-environment)
    (scheme-libraries syntax import-specs)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax library-loaders)
    (scheme-libraries syntax library-collections)
    (scheme-libraries syntax $serializing))

  (define-record-type stdlib
    (nongenerative stdlib-78444666-3654-446f-8cb5-5d2764763191)
    (sealed #t)
    (fields name pred system?))

  (define make-stdlibs-collection-datum
    (lambda (loc stdlib*)
      (define who 'stdlibs-collection)
      (define visible-libs (make-eq-hashtable))
      (define visible?
        (lambda (lib)
          (hashtable-contains? visible-libs lib)))
      (define system?
        (lambda (lib) #t))
      (parameterize ([current-library-loader (make-default-library-loader loc)]
                     [current-library-collection (bootstrap-library-collection)])
        (for-each
          (lambda (stdlib)
            (define name (stdlib-name stdlib))
            ;; TODO: Check whether load-library is the correct procedure here.
            ;; Need something like ensure-loaded.
            (cond
             [(load-library name (stdlib-pred stdlib))
              => (lambda (lib)
                   (hashtable-set! visible-libs lib #t)
                   (library-set! name lib))]
             [else
              (assertion-violation who "library not found" name)]))
          stdlib*)
        (library-collection->datum (current-library-collection) system? visible?))))



  )
