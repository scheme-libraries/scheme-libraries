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
    (scheme-libraries syntax $marks)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax $ribs)
    )

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
        (let ([lib* (library-list)])
          (define lib-expr* (map make-library-expr lib*))
          #`(library-collection #,@lib-expr*)



          ;; The stdlibs are now in the library collection.  Now
          ;; serialize the library collection.
          ;; what to do with the non-visible collections?
          ;; -> what did I mean with system?
          ;;
          ;; some libraries may be needed by instantiation of other libraries.
          ;; do we want all libraries initialized?
          ;; -> no, this is not what r6rs says.  but it could still work for stdlibs... ?
          ;; -> each library knows which have to be inven

          ))))

  (define make-library-expr
    (lambda (lib)
      ;; TODO
      #`(make-library #,(syntax-quote (library-name lib))
                      #,(syntax-quote (library-version lib))
                      #,(make-export-expr (library-exports lib))
                      '#()              ;invoke reqs
                      #f                ;invoker
                      )))

  ;; we need to serialize marks!

  (define make-export-expr
    (lambda (exports)
      #`(let ([rib (make-rib)])
          #,@(rib-map
              (lambda (n m l/p)
                #`(rib-set! rib
                            #,(syntax-quote name)
                            (datum->mark #,(syntax-quote (mark->datum m)))
                            (datum->label/props #,(syntax-quote (label/props->datum l/p)))))
              exports)
          rib)))

  )
