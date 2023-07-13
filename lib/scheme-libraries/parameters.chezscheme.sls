#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries parameters)
  (export
    make-parameter
    parameterize)
  (import
    (only (chezscheme)
      make-parameter
      parameterize)))
