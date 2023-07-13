#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries thread-parameters)
  (export
    make-thread-parameter)
  (import
    (only (chezscheme)
      make-thread-parameter)))
