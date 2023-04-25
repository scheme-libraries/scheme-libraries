#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

;;; XXX: Integrate into stdlib-collections.

(library (scheme-libraries syntax $make-stdlibs-collection-expr)
  (export
    make-stdlibs-collection-expr
    stdlib)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries parameters)
    (scheme-libraries syntax library-loaders)
    (scheme-libraries syntax library-collections))

  (define-record-type stdlib
    (nongenerative stdlib-78444666-3654-446f-8cb5-5d2764763191)
    (sealed #t)
    (fields name pred system? visible?))

  (define make-stdlibs-collection-expr
    (lambda (loc stdlib*)
      (let ([loader (make-default-library-loader loc)])
        (parameterize ([current-library-collection (bootstrap-library-collection)])
          (for-each
            (lambda (stdlib)
              (load-library name pred))
            stdlib*)
          ;; The stdlibs are now in the library collection.  Now
          ;; serialize the library collection.
          ;; what to do with the non-visible collections?
          ;;
          ;; some libraries may be needed by instantiation of other libraries.
          ;; do we want all libraries initialized?
          ;; -> no, this is not what r6rs says.  but it could still work for stdlibs... ?
          ;; -> each library knows which have to be inven

          ))))
  )
