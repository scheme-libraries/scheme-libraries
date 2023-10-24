#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs hashtables (6))
  (export
    ;; Constructors
    make-eq-hashtable
    make-eqv-hashtable
    make-hashtable
    ;; Procedures
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
    ;; Inspection
    hashtable-equivalence-function
    hashtable-hash-function
    hashtable-mutable?
    ;; Hash functions
    equal-hash
    string-hash
    string-ci-hash
    symbol-hash)
  (import
    ($system)))
