#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax library-locators)
  (export
    make-library-locator
    library-locator?
    library-locator-search)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries continuation-prompts)
    (scheme-libraries reading readers))

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
      (unless (library-locator? loc)
        (assertion-violation who "invalid library locator argument" loc))
      (let ([tag (make-continuation-prompt-tag 'library-locator-search)])
        (define locate-in-file
          (lambda (filename)
            (guard (c [(i/o-file-does-not-exist-error? c)])
              (call-with-input-file filename
                (lambda (in)
                  (let ([reader (make-reader in filename)])
                    (do ([form (reader-get-annotated-datum reader)])
                        ((eof-object? form))
                      (call/cc
                       (lambda (k)
                         (abort-current-continuation tag
                           (lambda ()
                             (succ form k))))))))))))
        (call-with-continuation-prompt
          (lambda ()
            (for-each
              (lambda (dir)
                (for-each
                  (lambda (ext)
                    (locate-in-file (library-name->filename dir name ext)))
                  (library-locator-extensions loc)))
              (library-locator-directories loc))
            (abort-current-continuation tag fail))
          tag (lambda (thunk) (thunk))))))

  (define library-name->filename
    (lambda (dir name ext)
      (let f ([dir dir] [name name])
	(let ([part (library-name-part->string (car name))])
	  (if (null? (cdr name))
	      (string-append dir (string-append part ext))
	      (f (string-append dir part "/") (cdr name)))))))

  (define library-name-part->string
    (lambda (part)
      (symbol->string part))))
