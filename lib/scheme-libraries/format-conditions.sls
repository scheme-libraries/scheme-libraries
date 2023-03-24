#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries format-conditions)
  (export
    &format
    make-format-condition
    format-condition?)
  (import
    (rnrs))

  (define-condition-type &format &condition
    make-format-condition format-condition?))
