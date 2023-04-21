#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax library-locators)
  (export
    library-locator?)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define-record-type library-locator
    (nongenerative library-locator-1f1b2c93-4e0f-45e4-bd04-ac084d6c18dd)
    (sealed #t)
    (fields directories extensions)
    (protocol
      (lambda (new)
        (define who 'make-library-locator)
        (lambda (dir* ext*)
          (unless (and (list? dir*)
                       (for-all string? dir*))
            (assertion-violation who "invalid directory list argument" dir*))
          (unless (and (list? ext*)
                       (for-all string? ext*))
            (assertion-violation who "invalid extension list argument" ext*))
          (new dir* ext*)))))

  (define/who library-locator-search
    (lambda (loc name pred? succ fail)
      (unless (locator? loc)
        (assertion-violation who "invalid library locator argument" loc))
      ((call/cc
        (lambda (abort)
          (let f ([dir* (library-locator-directories loc)])
            (if (pair? dir*)
                (let g ([ext* (library-locator-extensions loc)])
                  (if (null? ext*)
                      (f (cdr dir*))
                      (or (locate-in-file name pred?
                                          (library-name->filename (car dir*) name (car ext*))
                                          succ fail)
                          (g (cdr ext*)))))
                fail)))))))

  (define locate-in-file
    (lambda (abort name pred? filename succ fail)
      (guard (exc [(i/o-file-does-not-exist-error? exc) #f])
        (call-with-input-file filename
          (lambda (in)
            (call/cc
             (lambda (k)
               (abort
                (lambda ()
                  (succ filename k)))))
            ;; try next.
            )))))

  ;; todo: provide call-to-abort...

  )
