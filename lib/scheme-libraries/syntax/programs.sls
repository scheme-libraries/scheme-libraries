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
    (scheme-libraries syntax import-specs)
    (scheme-libraries syntax $ribs)
    (scheme-libraries syntax $ribcages)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects))

  (define/who load-program
    (lambda (filename)
      (unless (filename? filename)
        (assertion-violation who "invalid filename argument" filename))
      (let ([x (read-program filename)])
        (define-values (imp* body*)
          (parse-program x))
        (define ribs (make-ribcage))
        (define rib (make-rib))
        (ribcage-add-barrier! ribs rib '())
        (for-each
          (lambda (imp)
            ;; TODO: Start with the standard library collection and
            ;; write it together with the program.  At least for WPO
            ;; compilation.
            (import-spec-import! imp rib #f))
          imp*)
        ;; FIXME: expand body!
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
