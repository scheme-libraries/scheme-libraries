#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs (6))
  (export
    define
    define-syntax
    lambda
    quote
    syntax
    syntax-case)
  (import
    (rnrs base)
    (rnrs syntax-case)))
