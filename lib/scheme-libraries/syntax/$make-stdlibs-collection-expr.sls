#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $make-stdlibs-collection-expr)
  (export
    make-stdlibs-collection-expr
    make-stdlib
    stdlib?
    stdlib-name
    stdlib-pred
    stdlib-system?
    stdlib-visible?)
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
    (fields name pred system? visible?))

  (define make-stdlibs-collection-expr
    (lambda (loc stdlib*)
      (define who 'stdlibs-collection)
      (parameterize ([current-library-loader (make-default-library-loader loc)]
                     [current-library-collection (bootstrap-library-collection)])
        (for-each
          (lambda (stdlib)
            (define name (stdlib-name stdlib))
            ;; TODO: Check whether load-library is the correct procedure here.
            ;; Need something like ensure-loaded.
            (cond
             [(load-library name (stdlib-pred stdlib))
              => (lambda (lib) (library-set! name lib))]
             [else
              (assertion-violation who "library not found" name)]))
          stdlib*)
        #`(datum->library-collection
           #,(syntax-quote (library-collection->datum (current-library-collection))))






          ;; The stdlibs are now in the library collection.  Now
          ;; serialize the library collection.
          ;; what to do with the non-visible collections?
          ;; -> what did I mean with system?
          ;;
          ;; some libraries may be needed by instantiation of other libraries.
          ;; do we want all libraries initialized?
          ;; -> no, this is not what r6rs says.  but it could still work for stdlibs... ?
          ;; -> each library knows which have to be inven

          )))



  )
