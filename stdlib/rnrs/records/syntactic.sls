#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs records syntactic (6))
  (export
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
    record-constructor-descriptor)
  (import
    ($system)
    ($syntax)))
