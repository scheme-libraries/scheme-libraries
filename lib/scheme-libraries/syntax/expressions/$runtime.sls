#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions $runtime)
  (export
    ;; Keywords
    begin
    cons
    lambda
    letrec
    letrec*
    memv
    if
    quote
    set!
    ;; Procedures
    append
    car
    cdr
    eq?
    equal?
    map
    memv
    reverse
    syntax-car
    syntax-cdr
    syntax-null?
    syntax-pair?
    syntax-split
    syntax-vector?
    syntax-vector->list
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
