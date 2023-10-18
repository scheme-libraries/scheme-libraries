#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs lists (6))
  (export
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
    cons*)

  (import
    ($system)))
