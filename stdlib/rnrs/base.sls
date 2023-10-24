#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs base (6))
  (export

    ;; Definitions
    define
    define-syntax

    ;; Expressions
    quote
    lambda
    if
    set!
    cond
    =>
    else
    case
    and
    or
    let
    let*
    letrec
    letrec*
    let-values
    let*-values
    begin

    ;; Equivalence predicates
    eqv?
    eq?
    equal?

    ;; Procedure predicate
    procedure?

    ;; Arithmetic
    number?
    complex?
    real?
    rational?
    integer?
    real-valued?
    rational-valued?
    integer-valued?
    exact?
    inexact?
    exact
    inexact
    =
    <
    >
    <=
    >=
    zero?
    positive?
    negative?
    odd?
    even?
    finite?
    infinite?
    nan?
    max
    min
    +
    *
    -
    /
    abs
    div-and-mod
    div
    mod
    div0-and-mod0
    div0
    mod0
    gcd
    lcm
    numerator
    denominator
    floor
    ceiling
    truncate
    round
    rationalize
    exp
    log
    sin
    cos
    tan
    asin
    acos
    atan
    sqrt
    exact-integer-sqrt
    expt
    make-rectangular
    make-polar
    real-part
    imag-part
    magnitude
    angle
    number->string
    string->number

    ;; Booleans
    not
    boolean?
    boolean=?

    ;; Pairs
    pair?
    cons
    car
    cdr
    caar
    cadr
    cdar
    cddr
    caaaar
    caaadr
    caaar
    caadar
    caaddr
    caadr
    cadaar
    cadadr
    cadar
    caddar
    cadddr
    caddr
    cdaaar
    cdaadr
    cdaar
    cdadar
    cdaddr
    cdadr
    cddaar
    cddadr
    cddar
    cdddar
    cddddr
    cdddr
    null?
    list?
    list
    length
    append
    reverse
    list-tail
    list-ref
    map
    for-each

    ;; Symbols
    symbol?
    symbol->string
    symbol=?
    string->symbol

    ;; Characters
    char?
    char->integer
    integer->char
    char=?
    char<?
    char>?
    char<=?
    char>=?

    ;; Strings
    string?
    make-string
    string
    string-length
    string-ref
    string=?
    string<?
    string>?
    string<=?
    string>=?
    substring
    string-append
    string->list
    list->string
    string-for-each
    string-copy

    ;; Vectors
    vector?
    make-vector
    vector
    vector-length
    vector-ref
    vector-set!
    vector->list
    list->vector
    vector-fill!
    vector-map
    vector-for-each

    ;; Errors and violations
    error
    assertion-violation
    assert

    ;; Control features
    apply
    call-with-current-continuation
    call/cc
    values
    call-with-values
    dynamic-wind

    ;; Quasiquotation
    quasiquote
    unquote
    unquote-splicing

    ;; Binding constructs for syntactic keywords
    let-syntax
    letrec-syntax

    ;; Macro transformers
    syntax-rules
    identifier-syntax)

  (import
    ($system)))
