#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions $runtime)
  (export
    ;; Keywords
    begin
    cons
    cons*
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
    list
    reverse
    syntax-car
    syntax-cdr
    syntax-null?
    syntax-pair?
    syntax-split
    syntax-list
    syntax-list->vector
    syntax-vector?
    syntax-vector->list
    values
    void
    (rename ($identifier? identifier?)
            ($free-identifier=? free-identifier=?)
            ($make-variable-transformer make-variable-transformer)
            (syntax-object->datum syntax->datum)
            (syntax-error syntax-violation))

    ;; DEBUG
    display
    newline)
  (import
    (rnrs)
    (scheme-libraries void)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax variable-transformers)
    (scheme-libraries syntax syntax-objects)))
