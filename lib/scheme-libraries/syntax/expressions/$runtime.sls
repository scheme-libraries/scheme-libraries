#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions $runtime)
  (export
    ;; Keywords
    begin
    case-lambda
    letrec
    letrec*
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
    bytevector-u8-ref
    bytevector-s8-ref
    bytevector-u8-set!
    bytevector-s8-set!
    bytevector->u8-list
    u8-list->bytevector
    bytevector-uint-ref
    bytevector-sint-ref
    bytevector-uint-set!
    bytevector-sint-set!
    bytevector->uint-list
    bytevector->sint-list
    uint-list->bytevector
    sint-list->bytevector
    bytevector-u16-ref
    bytevector-s16-ref
    bytevector-u16-native-ref
    bytevector-s16-native-ref
    bytevector-u16-set!
    bytevector-s16-set!
    bytevector-u16-native-set!
    bytevector-s16-native-set!
    bytevector-u32-ref
    bytevector-s32-ref
    bytevector-u32-native-ref
    bytevector-s32-native-ref
    bytevector-u32-set!
    bytevector-s32-set!
    bytevector-u32-native-set!
    bytevector-s32-native-set!
    bytevector-u64-ref
    bytevector-s64-ref
    bytevector-u64-native-ref
    bytevector-s64-native-ref
    bytevector-u64-set!
    bytevector-s64-set!
    bytevector-u64-native-set!
    bytevector-s64-native-set!
    bytevector-ieee-single-native-ref
    bytevector-ieee-single-ref
    bytevector-ieee-double-native-ref
    bytevector-ieee-double-ref
    bytevector-ieee-single-native-set!
    bytevector-ieee-single-set!
    bytevector-ieee-double-native-set!
    bytevector-ieee-double-set!
    string->utf8
    string->utf16
    string->utf32
    utf8->string
    utf16->string
    utf32->string

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

    ;; (rnrs sorting)
    list-sort
    vector-sort
    vector-sort!

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
