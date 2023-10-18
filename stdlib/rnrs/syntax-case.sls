#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs syntax-case (6))
  (export

    ;; Transformers
    make-variable-transformer

    ;; Parsing input and producing output
    syntax-case
    _
    ...
    syntax

    ;; Identifier predicates
    identifier?
    bound-identifier=?
    free-identifier=?

    ;; Syntax-object and datum conversions
    syntax->datum
    datum->syntax

    ;; Generating lists of temporaries
    generate-temporaries

    ;; Derived forms and procedures
    with-syntax
    quasisyntax
    unsyntax
    unsyntax-splicing

    ;; Syntax violations
    syntax-violation)
  (import
    ($system)))
