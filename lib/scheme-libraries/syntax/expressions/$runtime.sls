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
    with-exception-handler
    raise
    raise-continuable

    ;; (rnrs conditions)
    condition
    simple-conditions
    condition?
    condition-predicate
    condition-accessor
    condition
    simple-conditions
    condition?
    condition-predicate
    condition-accessor
    make-message-condition
    message-condition?
    condition-message
    make-serious-condition
    serious-condition?
    make-error
    error?
    make-violation
    violation?
    make-irritants-condition
    irritants-condition?
    condition-irritants
    make-who-condition
    who-condition?
    condition-who
    make-non-continuable-violation
    non-continuable-violation?
    make-implementation-restriction-violation
    implementation-restriction-violation?
    (rename
      (make-lexical-error make-lexical-violation)
      (lexical-error? lexical-violation?)
      (make-syntax-error make-syntax-violation)
      (syntax-error? syntax-violation?)
      (syntax-error-form syntax-violation-form)
      (syntax-error-subform syntax-violation-subform)
      (make-undefined-error make-undefined-violation)
      (undefined-error? undefined-violation?))

    ;; (rnrs io ports)
    buffer-mode?
    latin-1-codec
    utf-8-codec
    utf-16-codec
    make-i/o-decoding-error
    i/o-decoding-error?
    make-i/o-encoding-error
    i/o-encoding-error?
    i/o-encoding-error-char
    native-eol-style
    make-transcoder
    native-transcoder
    transcoder-codec
    transcoder-eol-style
    transcoder-error-handling-mode
    bytevector->string
    string->bytevector
    eof-object
    eof-object?
    port?
    port-transcoder
    textual-port?
    binary-port?
    transcoded-port
    port-has-port-position?
    port-position
    port-has-set-port-position!?
    set-port-position!
    close-port
    call-with-port
    input-port?
    port-eof?
    open-file-input-port
    open-bytevector-input-port
    open-string-input-port
    standard-input-port
    current-input-port
    make-custom-binary-input-port
    make-custom-textual-input-port
    get-u8
    lookahead-u8
    get-bytevector-n
    get-bytevector-n!
    get-bytevector-some
    get-bytevector-all
    get-char
    lookahead-char
    get-string-n
    get-string-n!
    get-string-all
    get-line
    get-datum
    output-port?
    flush-output-port
    output-port-buffer-mode
    open-file-output-port
    open-bytevector-output-port
    call-with-bytevector-output-port
    open-string-output-port
    call-with-string-output-port
    standard-output-port
    standard-error-port
    current-output-port
    current-error-port
    make-custom-binary-output-port
    make-custom-textual-output-port
    put-u8
    put-bytevector
    put-char
    put-string
    put-datum
    open-file-input/output-port
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port

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
    make-no-infinities-violation
    no-infinities-violation?
    make-no-nans-violation
    no-nans-violation?

    ;; (rnrs arithmetic bitwise)
    bitwise-not
    bitwise-and
    bitwise-ior
    bitwise-xor
    bitwise-if
    bitwise-bit-count
    bitwise-length
    bitwise-first-bit-set
    bitwise-bit-set?
    bitwise-copy-bit
    bitwise-bit-field
    bitwise-copy-bit-field
    bitwise-arithmetic-shift
    bitwise-arithmetic-shift-left
    bitwise-arithmetic-shift-right
    bitwise-rotate-bit-field
    bitwise-reverse-bit-field

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

    ;; (rnrs mutable-pairs)
    set-car!
    set-cdr!

    ;; (rnrs mutable-strings)
    string-set!
    string-fill!

    ;; I/O error types
    make-i/o-error
    i/o-error?
    make-i/o-read-error
    i/o-read-error?
    make-i/o-write-error
    i/o-write-error?
    make-i/o-invalid-position-error
    i/o-invalid-position-error?
    i/o-error-position
    make-i/o-filename-error
    i/o-filename-error?
    i/o-error-filename
    make-i/o-file-protection-error
    i/o-file-protection-error?
    make-i/o-file-is-read-only-error
    i/o-file-is-read-only-error?
    make-i/o-file-already-exists-error
    i/o-file-already-exists-error?
    make-i/o-file-does-not-exist-error
    i/o-file-does-not-exist-error?
    make-i/o-port-error
    i/o-port-error?
    i/o-error-port

    set-box!
    location-box
    location-box-set!
    keyword-binding-transformer-set!
    property-set!
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



    ;; (scheme-libraries)
    (rename
      ($construct-name construct-name))
    uid

    ;; File options
    $file-options

    ;; Conditions
    condition-rtd
    message-rtd
    warning-rtd
    serious-rtd
    error-rtd
    violation-rtd
    assertion-rtd
    irritants-rtd
    who-rtd
    non-continuable-rtd
    implementation-restriction-rtd
    lexical-rtd
    syntax-rtd
    undefined-rtd
    i/o-rtd
    i/o-read-rtd
    i/o-write-rtd
    i/o-invalid-position-rtd
    i/o-filename-rtd
    i/o-file-protection-rtd
    i/o-file-is-read-only-rtd
    i/o-file-already-exists-rtd
    i/o-file-does-not-exist-rtd
    i/o-port-rtd
    i/o-decoding-rtd
    i/o-encoding-rtd
    no-infinities-rtd
    no-nans-rtd)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (scheme-libraries boxes)
    (scheme-libraries void)
    (scheme-libraries uuid)
    (scheme-libraries reading tokenizers)
    (scheme-libraries syntax $helpers)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax identifiers)
    (scheme-libraries syntax variable-transformers)
    (scheme-libraries syntax syntax-objects))

  ;; File options

  (define $file-options
    (lambda ()
      (enum-set->list (enum-set-universe (file-options)))))

  ;; Conditions

  (define (condition-rtd) (record-type-descriptor &condition))
  (define (message-rtd) (record-type-descriptor &message))
  (define (warning-rtd) (record-type-descriptor &warning))
  (define (serious-rtd) (record-type-descriptor &serious))
  (define (error-rtd) (record-type-descriptor &error))
  (define (violation-rtd) (record-type-descriptor &violation))
  (define (assertion-rtd) (record-type-descriptor &assertion))
  (define (irritants-rtd) (record-type-descriptor &irritants))
  (define (who-rtd) (record-type-descriptor &who))
  (define (non-continuable-rtd) (record-type-descriptor &non-continuable))
  (define (implementation-restriction-rtd) (record-type-descriptor &implementation-restriction))
  (define (lexical-rtd) (record-type-descriptor &lexical-error))
  (define (syntax-rtd) (record-type-descriptor $&syntax))
  (define (undefined-rtd) (record-type-descriptor $&undefined))

  (define (i/o-rtd) (record-type-descriptor &i/o))
  (define (i/o-read-rtd) (record-type-descriptor &i/o-read))
  (define (i/o-write-rtd) (record-type-descriptor &i/o-write))
  (define (i/o-invalid-position-rtd) (record-type-descriptor &i/o-invalid-position))
  (define (i/o-filename-rtd) (record-type-descriptor &i/o-filename))
  (define (i/o-file-protection-rtd) (record-type-descriptor &i/o-file-protection))
  (define (i/o-file-is-read-only-rtd) (record-type-descriptor &i/o-file-is-read-only))
  (define (i/o-file-already-exists-rtd) (record-type-descriptor &i/o-file-already-exists))
  (define (i/o-file-does-not-exist-rtd) (record-type-descriptor &i/o-file-does-not-exist))
  (define (i/o-port-rtd) (record-type-descriptor &i/o-port))

  (define (i/o-decoding-rtd) (record-type-descriptor &i/o-decoding))
  (define (i/o-encoding-rtd) (record-type-descriptor &i/o-encoding))

  (define (no-infinities-rtd) (record-type-descriptor &no-infinities))
  (define (no-nans-rtd) (record-type-descriptor &no-nans))

  )
