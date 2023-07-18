#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

;;; XXX: Integrate into stdlib-collections.

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
    (scheme-libraries parameters)
    (scheme-libraries syntax bootstrap-environment)
    (scheme-libraries syntax import-specs)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax library-loaders)
    (scheme-libraries syntax library-collections))

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
            (unless (load-library name (stdlib-pred stdlib))
              (assertion-violation who "library not found" name)))
          stdlib*)



        ;; The stdlibs are now in the library collection.  Now
        ;; serialize the library collection.
        ;; what to do with the non-visible collections?
        ;; -> what did I mean with system?
        ;;
        ;; some libraries may be needed by instantiation of other libraries.
        ;; do we want all libraries initialized?
        ;; -> no, this is not what r6rs says.  but it could still work for stdlibs... ?
        ;; -> each library knows which have to be inven

        (let* ([lib* (map
                       (lambda (stdlib)
                         ;; FIXME: In library loaders, the stdlib is not entered into the tables...
                         (assert (library-ref (stdlib-name stdlib) #f)))
                       stdlib*)]
               [name-stx* (map (lambda (lib)
                                 (datum->syntax #'here (library-name lib)))
                               lib*)])
          (with-syntax ([(name ...) name-stx*]
                        [(lib ...)
                         (map (lambda (name-stx lib)
                                #`(make-library '#,name-stx
                                                #,(library-version lib)
                                                (make-rib) ;fixme: exports!
                                                '#() ;invoke-requirements
                                                #f   ;invoker
                                                ))
                              name-stx* lib*)])
            #'(parameterize ([current-library-collection (empty-library-collection)])
                (library-set! 'name lib)
                ...
                (current-library-collection))))))))
