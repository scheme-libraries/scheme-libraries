#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries format-conditions)
  (export
    &format
    make-format-condition
    format-condition?)
  (import
    (chezscheme)))
