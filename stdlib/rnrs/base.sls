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
    values
    )
  (import
    (rename ($system)
      (values $values)))

  ;; todo: how to define variadic procedures with primitives?
  ;; todo: how to compile everything together (including memv ...)

  (define values
    (lambda x*


      ))

  )
