#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs (6))
  (export

    ;; (rnrs base)
    define
    define-syntax
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
    eqv?
    eq?
    equal?
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
    not
    boolean?
    boolean=?
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
    symbol?
    symbol->string
    symbol=?
    string->symbol
    char?
    char->integer
    integer->char
    char=?
    char<?
    char>?
    char<=?
    char>=?
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
    error
    assertion-violation
    assert
    apply
    call-with-current-continuation
    call/cc
    values
    call-with-values
    dynamic-wind
    quasiquote
    unquote
    unquote-splicing
    let-syntax
    letrec-syntax
    syntax-rules
    identifier-syntax

    ;; (rnrs unicode)
    char-upcase
    char-downcase
    char-titlecase
    char-foldcase
    char-ci=?
    char-ci<?
    char-ci>?
    char-ci<=?
    char-ci>=?
    char-alphabetic?
    char-numeric?
    char-whitespace?
    char-upper-case?
    char-lower-case?
    char-title-case?
    char-general-category
    string-upcase
    string-downcase
    string-titlecase
    string-foldcase
    string-ci=?
    string-ci<?
    string-ci>?
    string-ci<=?
    string-ci>=?
    string-normalize-nfd
    string-normalize-nfkd
    string-normalize-nfc
    string-normalize-nfkc

    ;; (rnrs syntax-case)
    syntax
    syntax-case)
  (import
    (rnrs base)
    (rnrs syntax-case)))
