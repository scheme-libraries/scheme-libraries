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

    ;; (rnrs base)
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

    ;; (rnrs bytevectors)
    native-endianness
    bytevector?
    make-bytevector
    bytevector-length
    bytevector=?
    bytevector-fill!
    bytevector-copy!
    bytevector-copy

    ;; (rnrs enums)
    make-enumeration
    enum-set-universe
    enum-set-indexer
    enum-set-constructor
    enum-set->list
    enum-set-member?
    enum-set-subset?
    enum-set=?
    enum-set-union
    enum-set-intersection
    enum-set-difference
    enum-set-complement
    enum-set-projection

    ;; (rnrs lists)
    find
    for-all
    exists
    filter
    partition
    fold-left
    fold-right
    remp
    remove
    remv
    remq
    memp
    member
    memv
    memq
    assp
    assoc
    assv
    assq
    cons*

    set-box!
    location-box
    location-box-set!
    keyword-binding-transformer-set!
    label-binding
    syntax-car
    syntax-cdr
    syntax-null?
    syntax-pair?
    syntax-split
    syntax-list
    syntax-list->vector
    syntax-vector?
    syntax-vector->list
    unbox
    void

    ;; (rnrs syntax-case)
    (rename
      ($make-variable-transformer make-variable-transformer)
      ($identifier? identifier?)
      ($bound-identifier=? bound-identifier=?)
      ($free-identifier=? free-identifier=?)
      (syntax-object->datum syntax->datum)
      (datum->syntax-object datum->syntax)
      ($generate-temporaries generate-temporaries)
      (syntax-error syntax-violation))

    ;; DEBUG
    write
    display
    newline)
  (import
    (rnrs)
    (scheme-libraries boxes)
    (scheme-libraries void)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax identifiers)
    (scheme-libraries syntax variable-transformers)
    (scheme-libraries syntax syntax-objects)))
