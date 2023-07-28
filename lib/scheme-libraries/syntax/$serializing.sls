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
      (let ([imports (library-imports lib)])
        (define import-table
          (let ([ht (make-eq-hashtable)])
            (define n (vector-length imports))
            (do ([i 0 (+ i 1)])
                ((= i n) ht)
              (hashtable-set! ht (vector-ref imports i) i))))
        (define library->index
          (lambda (lib)
            (assert (library? lib))
            (assert (hashtable-ref import-table lib #f))))
        (extend-backquote here
          `($library (,@(library-name lib) ,(library-version lib))
             (uid ,(library-uid lib))
             (import ,@(vector->list
                        (vector-map
                         (lambda (implib)
                           `((,@(library-name imp) ,(library-version implib))
                             ,(library-uid implib)))
                         (library-imports lib))))
             (visit-requirements ,@(vector->list
                                    (vector-map library->index (visit-requirements lib))))
             (invoke-requirements ,@(vector->list
                                     (vector-map library->index (invoke-requirements lib))))
             (export ,@(rib-map
                        (lambda (n m l/p)
                          (assert (null? m))
                          `(,n ,(label/props->datum l/p)))
                        (library-exports lib)))
             (environment ,env* ...)
             (visit-commands ,@viscmd*)
             (invoke-definitions ,@invdef*)



             )))))

  (define/who datum->library
    (lambda (e)
      (match e
        [($library (,name ... ,version)
           (uid ,uid)
           (import ,imp* ...)
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
