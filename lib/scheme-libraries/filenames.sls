#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries filenames)
  (export
    filename?)
  (import
    (rnrs))

  (define filename?
    (lambda (obj)
      (string? obj))))
