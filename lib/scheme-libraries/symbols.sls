#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries symbols)
  (export
    symbol-prefix?)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries strings))

  (define/who symbol-prefix?
    (lambda (sym prefix)
      (unless (symbol? sym)
        (assertion-violation who "invalid symbol argument" sym))
      (unless (string? prefix)
        (assertion-violation who "invalid prefix argument" prefix))
      (string-prefix? (symbol->string sym) prefix))))
