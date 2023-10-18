#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs records procedural (6))
  (export
    make-record-type-descriptor
    record-type-descriptor?
    make-record-constructor-descriptor
    record-constructor
    record-predicate
    record-accessor
    record-mutator)
  (import
    ($system)))
