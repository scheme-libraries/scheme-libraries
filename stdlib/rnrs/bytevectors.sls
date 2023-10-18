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

    ;; Operations on integers of arbitrary size

    ;; Operations on 16-bit integers

    ;; Operations on 32-bit integers

    ;; Operations on 64-bit integers

    ;; Operations on IEEE-754 representations

    ;; Operations on strings

    )

  (import
    ($system)
    ($syntax)))
