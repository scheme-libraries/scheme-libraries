#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs enums (6))
  (export
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
    ($system)
    ($syntax)))
