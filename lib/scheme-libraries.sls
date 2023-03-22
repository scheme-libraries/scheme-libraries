#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries)
  (export
    define/who
    define-syntax/who
    define-values
    make-parameter
    make-thread-parameter
    with-implicit)
  (import
    (scheme-libraries define-who)
    (scheme-libraries define-values)
    (scheme-libraries parameters)
    (scheme-libraries thread-parameters)
    (scheme-libraries with-implicit))

  )
