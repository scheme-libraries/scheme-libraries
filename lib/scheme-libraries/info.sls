#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries info)
  (export
    info)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who))

  (define info
    (lambda (fmt . arg*)
      (let ([p (current-error-port)])
        (display "[info] " p)
        (display (apply format fmt arg*) p)
        (newline p)))))
