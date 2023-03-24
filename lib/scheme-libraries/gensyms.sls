#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries gensyms)
  (export
    gensym)
  (import
    (rnrs)
    (scheme-libraries counters)
    (scheme-libraries define-who))

  (define counter
    (make-counter))

  (define/who gensym
    (case-lambda
      [(prefix separator)
       (unless (string? prefix)
         (assertion-violation who "invalid prefix argument" prefix))
       (unless (string? separator)
         (assertion-violation who "invalid separator argument" separator))
       (string->symbol
        (string-append prefix separator (number->string (counter))))]
      [(prefix) (gensym prefix "")]
      [() (gensym "g")])))
