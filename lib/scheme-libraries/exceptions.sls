#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries exceptions)
  (export
    assertion-violationf
    errorf)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings))

  (define assertion-violationf
    (lambda (who cntl . arg*)
      (assertion-violation who (apply format cntl arg*))))

  (define errorf
    (lambda (who cntl . arg*)
      (error who (apply format cntl arg*)))))
