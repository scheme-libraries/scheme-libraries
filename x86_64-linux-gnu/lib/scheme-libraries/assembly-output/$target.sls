#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries assembly-output $target)
  (export
    label->string)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who))

  (define/who label->string
    (lambda (label)
      (cond
       [(string? label) label]
       [(symbol? label) (format ".L~s" (symbol->suffix label))]
       [else
        (assertion-violation who "invalid label argument" label)])))
  )
