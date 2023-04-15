#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions $runtime)
  (export
    ;; Keywords
    begin
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
    syntax-car
    syntax-cdr
    syntax-null?
    syntax-pair?
    void
    (rename ($identifier? identifier?)
            ($free-identifier=? free-identifier=?)
            (syntax-object->datum syntax->datum)
            (syntax-error syntax-violation))

    ;; DEBUG
    display
    newline)
  (import
    (rnrs)
    (scheme-libraries void)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax syntax-objects)))
