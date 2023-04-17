#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax variable-transformers)
  (export
    (rename (make-variable-transformer $make-variable-transformer))
    variable-transformer?
    variable-transformer-proc)
  (import
    (except (rnrs)
            make-variable-transformer))

  (define-record-type variable-transformer
    (nongenerative variable-transformer-a9ffd541-8ce7-4d23-809f-0d59f7860b0f)
    (sealed #t)
    (fields proc)
    (protocol
      (lambda (new)
        (define who 'make-variable-transformer)
        (lambda (proc)
          (unless (procedure? proc)
            (assertion-violation who "invalid procedure argument" proc))
          (new proc)))))
  )
