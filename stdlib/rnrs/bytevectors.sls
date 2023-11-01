#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs bytevectors (6))
  (export

    ;; General operations
    endianness
    native-endianness
    bytevector?
    make-bytevector
    bytevector-length
    bytevector=?
    bytevector-fill!
    bytevector-copy!
    bytevector-copy

    ;; Operations on bytes and octets
    bytevector-u8-ref
    bytevector-s8-ref
    bytevector-u8-set!
    bytevector-s8-set!
    bytevector->u8-list
    u8-list->bytevector

    ;; Operations on integers of arbitrary size
    bytevector-uint-ref
    bytevector-sint-ref
    bytevector-uint-set!
    bytevector-sint-set!
    bytevector->uint-list
    bytevector->sint-list
    uint-list->bytevector
    sint-list->bytevector

    ;; Operations on 16-bit integers
    bytevector-u16-ref
    bytevector-s16-ref
    bytevector-u16-native-ref
    bytevector-s16-native-ref
    bytevector-u16-set!
    bytevector-s16-set!
    bytevector-u16-native-set!
    bytevector-s16-native-set!

    ;; Operations on 32-bit integers
    bytevector-u32-ref
    bytevector-s32-ref
    bytevector-u32-native-ref
    bytevector-s32-native-ref
    bytevector-u32-set!
    bytevector-s32-set!
    bytevector-u32-native-set!
    bytevector-s32-native-set!

    ;; Operations on 64-bit integers
    bytevector-u64-ref
    bytevector-s64-ref
    bytevector-u64-native-ref
    bytevector-s64-native-ref
    bytevector-u64-set!
    bytevector-s64-set!
    bytevector-u64-native-set!
    bytevector-s64-native-set!

    ;; Operations on IEEE-754 representations
    bytevector-ieee-single-native-ref
    bytevector-ieee-single-ref
    bytevector-ieee-double-native-ref
    bytevector-ieee-double-ref
    bytevector-ieee-single-native-set!
    bytevector-ieee-single-set!
    bytevector-ieee-double-native-set!
    bytevector-ieee-double-set!

    ;; Operations on strings
    string->utf8
    string->utf16
    string->utf32
    utf8->string
    utf16->string
    utf32->string)

  (import
    ($system)
    (rnrs enums)
    (scheme-libraries with-implicit))

  (let-syntax ([define-endianness
                 (with-syntax ([(option ...)
                                (datum->syntax #'here (cons (native-endianness) '(big little)))])
                   (lambda (x)
                     (syntax-case x ()
                       [(k)
                        (with-implicit (k endianness)
                          #'(define-enumeration endianness
                              (option ...)
                              endiannesses))])))])
    (define-endianness)))
