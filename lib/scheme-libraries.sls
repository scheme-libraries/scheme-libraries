#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries)
  (export
    construct-name
    define-auxiliary-syntax
    define/who
    define-syntax/who
    define-values
    ellipsis?
    make-parameter
    make-thread-parameter
    symbolic-identifier=?
    with-implicit

    ;; (scheme-libraries basic-format-strings)
    format

    ;; (scheme-libraries exceptions)
    assertion-violationf
    errorf

    ;; (scheme-libraries lists)
    length+
    split-at

    ;; (scheme-libraries match)
    match
    unquote
    ...
    _
    ->
    guard

    ;; (scheme-libraries numbers)
    exact-integer?
    exact-positive-integer?
    exact-nonnegative-integer?
    nonnegative-fixnum?

    ;; (scheme-libraries record-writer)
    record-writer)
  (import
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-auxiliary-syntax)
    (scheme-libraries define-who)
    (scheme-libraries define-values)
    (scheme-libraries exceptions)
    (scheme-libraries parameters)
    (scheme-libraries helpers)
    (scheme-libraries lists)
    (scheme-libraries match)
    (scheme-libraries numbers)
    (scheme-libraries record-writer)
    (scheme-libraries thread-parameters)
    (scheme-libraries with-implicit)))
