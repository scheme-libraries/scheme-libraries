#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries ports)
  (export
    textual-input-port?
    textual-output-port?)
  (import
    (rnrs))

  (define textual-input-port?
    (lambda (obj)
      (and (textual-port? obj)
           (input-port? obj))))

  (define textual-output-port?
    (lambda (obj)
      (and (textual-port? obj)
           (output-port? obj)))))
