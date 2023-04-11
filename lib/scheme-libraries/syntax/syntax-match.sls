#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax syntax-match)
  (export
    syntax-match
    syntax-extend-backquote
    unquote
    ...
    _
    ->
    guard)
  (import
    (rnrs)
    (scheme-libraries define-match)
    (rename (scheme-libraries syntax syntax-objects)
            (syntax-car match-car)
            (syntax-cdr match-cdr)
            (syntax-pair? match-pair?)
            (syntax-null? match-null?)
            (syntax-vector? match-vector?)
            (syntax-vector->list match-syntax-vector->list)))

  ;; TODO: match-quote

  (define-match (syntax-match syntax-extend-backquote)))
