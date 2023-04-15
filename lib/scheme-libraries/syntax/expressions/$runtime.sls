#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions $runtime)
  (export
    ;; Keywords
    lambda
    letrec
    letrec*
    memv
    if
    quote
    set!
    ;; Procedures
    equal?
    memv
    void
    syntax->datum
    (rename (syntax-error syntax-violation)))
  (import
    (rnrs)
    (scheme-libraries void)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax syntax-objects)))
