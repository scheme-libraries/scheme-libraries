#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs base (6))
  (export
    ;; Definitions
    define
    define-syntax
    ;; Expressions
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

    list
    values)
  (import
    ($system))

  )
