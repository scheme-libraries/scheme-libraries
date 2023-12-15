#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries match)
  (export
    match
    unquote
    ...
    _
    ->
    guard
    extend-backquote)
  (import
    (rnrs)
    (scheme-libraries define-match)
    (prefix (only (rnrs)
                  equal?
                  quote
                  car
                  cdr
                  null?
                  pair?
                  vector?
                  vector->list)
            match-))

  (define-match (match extend-backquote)))

;; Local Variables:
;; mode: scheme
;; End:
