#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries thread-parameters)
  (export
    make-thread-parameter)
  (import
    (rnrs)
    (scheme-libraries parameters))

  (define make-thread-parameter make-parameter))
