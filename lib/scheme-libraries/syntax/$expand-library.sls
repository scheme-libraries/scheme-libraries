#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $expand-library)
  (export
    expand-library)
  (import
    (rnrs)
    (scheme-libraries syntax import-specs)
    (scheme-libraries syntax expand)
    (scheme-libraries syntax syntax-objects))

  ;; Libraries

  (define expand-library
    (lambda (name ver exp* imp* body*)
      (let ([ribs (make-ribcage)])
        (ribcage-add-barrier! ribs '())
        (for-each
          (lambda (imp)
            (import-spec-import! imp ribs))
          imp*)
        ;; TODO: Add ribs to body.



        ;; FIXME
        (assert #f))))



  )
