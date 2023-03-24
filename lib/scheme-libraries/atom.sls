#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries atom)
  (export
    atom?)
  (import
    (rnrs))

  (define atom?
    (lambda (obj)
      (or (boolean? obj)
          (bytevector? obj)
          (char? obj)
          (null? obj)
          (number? obj)
          (symbol? obj)
          (string? obj)))))
