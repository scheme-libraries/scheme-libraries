#!r6rs

(library (scheme-libraries syntax programs)
  (export
    load-program)
  (import
    (rnrs)
    (scheme-libraries define-values)
    (scheme-libraries define-who)
    (scheme-libraries filenames)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries reading readers)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax expand)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects))

  (define/who load-program
    (lambda (filename)
      (unless (filename? filename)
        (assertion-violation who "invalid filename argument" filename))
      (let ([x (read-program filename)])
        (define-values (imp* body*)
          (parse-program x))
        (define-values ([(e invreqs)
                         (expand-program imp* body*)]))
        (vector-for-each library-invoke! invreqs)
        (display e)
        (newline)



        ;; Do something with the expanded program.
        (assert #f))))

  (define read-program
    (lambda (filename)
      (assert (filename? filename))
      (call-with-input-file filename
        (lambda (in)
          (define reader (make-reader in filename))
          (define e*
            (let f ()
              (define e (reader-get-annotated-datum reader))
              (if (eof-object? e)
                  '()
                  (cons e (f)))))
          (annotated-datum->syntax-object (make-annotated-list e* #f)
                                          (system-environment))))))

  (define parse-program
    (lambda (x)
      (syntax-match x
        [((import ,imp-spec* ...)
          ,body* ...)
         (values imp-spec* body*)]
        [,x (syntax-error #f "invalid program syntax" x)]))))
