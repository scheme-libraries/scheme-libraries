#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs base (6))
  (export
    ;; 11.2
    define
    define-syntax
    ;; 11.4
    quote
    lambda
    if
    set!
    cond
    case
    and
    or
    let
    let*
    letrec
    letrec*
    let-values
    let*-values
    list values
    )
  (import
    ($system)))
