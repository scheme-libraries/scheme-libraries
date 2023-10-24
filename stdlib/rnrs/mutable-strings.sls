#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs mutable-strings (6))
  (export
    string-set!
    string-fill!)
  (import
    ($system)))
