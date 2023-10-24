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

    ;; (rnrs unicode)
    endianness
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

    ;; (rnrs control)
    when
    unless
    do
    case-lambda

    ;; (rnrs records syntactic)
    define-record-type
    fields
    mutable
    immutable
    parent
    protocol
    sealed
    opaque
    nongenerative
    parent-rtd
    record-type-descriptor
    record-constructor-descriptor

    ;; (rnrs records procedural)
    make-record-type-descriptor
    record-type-descriptor?
    make-record-constructor-descriptor
    record-constructor
    record-predicate
    record-accessor
    record-mutator

    ;; (rnrs records inspection)
    record?
    record-rtd
    record-type-name
    record-type-parent
    record-type-uid
    record-type-generative?
    record-type-sealed?
    record-type-opaque?
    record-type-field-names
    record-field-mutable?

    ;; (rnrs exceptions)
    guard
    with-exception-handler
    raise
    raise-continuable

    ;; (rnrs conditions)
    &condition
    condition
    simple-conditions
    condition?
    condition-predicate
    condition-accessor
    define-condition-type
    &message
    make-message-condition
    message-condition?
    condition-message
    &warning
    make-warning-condition
    warning-condition?
    &serious
    make-serious-condition
    serious-condition?
    &error
    make-error
    error?
    &violation
    make-violation
    violation?
    &irritants
    make-irritants-condition
    irritants-condition?
    condition-irritants
    &who
    make-who-condition
    who-condition?
    condition-who
    &non-continuable
    make-non-continuable-violation
    non-continuable-violation?
    &implementation-restriction
    make-implementation-restriction-violation
    implementation-restriction-violation?
    &lexical
    make-lexical-violation
    lexical-violation?
    &syntax
    make-syntax-violation
    syntax-violation?
    syntax-violation-form
    syntax-violation-subform
    &undefined
    make-undefined-violation
    undefined-violation?

    ;; (rnrs io simple)
    eof-object
    call-with-input-file
    call-with-output-file
    input-port?
    output-port?
    current-input-port
    current-output-port
    current-error-port
    with-input-from-file
    with-output-to-file
    open-input-file
    open-output-file
    close-input-port
    close-output-port
    read-char
    peek-char
    read
    write-char
    newline
    display
    write

    ;; (rnrs files)
    file-exists?
    delete-file

    ;; (rnrs programs)
    command-line
    exit

    ;; (rnrs arithmetic fixnums)
    fixnum?
    fixnum-width
    least-fixnum
    greatest-fixnum
    fx=?
    fx>?
    fx<?
    fx>=?
    fx<=?
    fxzero?
    fxpositive?
    fxnegative?
    fxodd?
    fxeven?
    fxmax
    fx+
    fx*
    fx-
    fxdiv-and-mod
    fxdiv
    fxmod
    fxdiv0-and-mod0
    fxdiv0
    fxmod0
    fx+/carry
    fx-/carry
    fx*/carry
    fxnot
    fxand
    fxior
    fxxor
    fxif
    fxbit-count
    fxlength
    fxfirst-bit-set
    fxbit-set?
    fxcopy-bit
    fxbit-field
    fxcopy-bit-field
    fxarithmetic-shift
    fxarithmetic-shift-left
    fxarithmetic-shift-right
    fxrotate-bit-field
    fxreverse-bit-field

    ;; (rnrs arithmetic flonums)
    flonum?
    real->flonum
    fl=?
    fl<?
    fl<=?
    fl>?
    fl>=?
    flinteger?
    flzero?
    flpositive?
    flnegative?
    flodd?
    fleven?
    flfinite?
    flinfinite?
    flnan?
    flmax
    flmin
    fl+
    fl*
    fl-
    fl/
    flabs
    fldiv-and-mod
    fldiv
    flmod
    fldiv0-and-mod0
    fldiv0
    flmod0
    flnumerator
    fldenominator
    flfloor
    flceiling
    fltruncate
    flround
    flexp
    fllog
    flsin
    flcos
    fltan
    flasin
    flacos
    flatan
    flsqrt
    flexpt
    fixnum->flonum
    &no-infinities
    make-no-infinities-violation
    no-infinities-violation?
    &no-nans
    make-no-nans-violation
    no-nans-violation?

    ;; (rnrs syntax-case)
    make-variable-transformer
    syntax-case
    _
    ...
    syntax
    identifier?
    bound-identifier=?
    free-identifier=?
    syntax->datum
    datum->syntax
    generate-temporaries
    with-syntax
    quasisyntax
    unsyntax
    unsyntax-splicing
    syntax-violation

    ;; (rnrs hashtables)
    make-eq-hashtable
    make-eqv-hashtable
    make-hashtable
    hashtable?
    hashtable-size
    hashtable-ref
    hashtable-set!
    hashtable-delete!
    hashtable-contains?
    hashtable-update!
    hashtable-copy
    hashtable-clear!
    hashtable-keys
    hashtable-entries
    hashtable-equivalence-function
    hashtable-hash-function
    hashtable-mutable?

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
    define-enumeration)
  (import
    (rnrs base)
    (rnrs unicode)
    (rnrs bytevectors)
    (rnrs lists)
    (rnrs sorting)
    (rnrs control)
    (rnrs records syntactic)
    (rnrs records procedural)
    (rnrs records inspection)
    (rnrs exceptions)
    (rnrs conditions)
    (rnrs io simple)
    (rnrs files)
    (rnrs programs)
    (rnrs arithmetic fixnums)
    (rnrs arithmetic flonums)
    (rnrs hashtables)
    (rnrs syntax-case)
    (rnrs enums)))
