#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $serializing)
  (export
    library->datum
    datum->library)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries match)
    (scheme-libraries syntax libraries))

  (define/who library->datum
    (lambda (lib)
      (assert (library? lib))
      (let (uid* (list #f))
        (define name (library-name lib))
        (define version (library-version lib))
        (define imp*
          (vector->list (vector-map library->uid (vector-map library-imports lib))))
        (define visreq*
          (vector->list (vector-map library->uid (vector-map library-imports lib))))
        (define invreq*
          (vector->list (vector-map library->uid (vector-map library-imports lib))))
        (define exp*
          ;; serialize exports!
          )
        (define viscmd*
          ;; serialize code
          (library-visit-commands lib))
        (define invdef*
          (library-invoke-definitions lib))
        (define e
          (extend-backquote here
            `($library (,@name ,version)
               (uid . ,uid*)
               (import ,imp* ...)
               (visit-requirements ,visreq* ...)
               (invoke-requirements ,invreq* ...)
               (export ,exp* ...)
               (environment ,env* ...)
               (visit-commands ,@viscmd*)
               (invoke-definitions ,@invdef*))))
        (define uid (equal-hash e))
        (cond
         [(library-uid lib)
          => (lambda (x)
               (assert (eqv? x uid)))]
         [else
          (library-uid-set! lib e)])
        (set-car! uid* uid)
        e)))

  (define library->uid
    (lambda (lib)
      (assert (library? lib))
      (library-serialize! lib)
      (library-uid lib)))

  ;; We don't want two library tables.

  (define library-serialize!
    (lambda (lib)
      (assert (library? lib))
      (unless (library-uid lib)
        (hashtable-set! ...)

        )))

  (define/who datum->library
    (lambda (e)
      (match e
        [($library (,name ... ,version)
           (uid ,uid)
           (import ,imp*)
           (visit-requirements ,visreq* ...)
           (invoke-requirements ,invreq* ...)
           (export ,exp* ...)
           (environment ,env* ...)
           (visit-code ,viscode* ...)
           (invoke-code ,invcode* ...))
         ;; FIXME
         (assert #f)]
        [,_
         (assertion-violation who "invalid expanded library definition" e)])))

  )
