#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions $runtime)
  (export
    lambda
    letrec
    letrec*
    memv
    if
    quote
    set!)
  (import
    (rnrs)))
