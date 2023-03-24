#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries display-condition)
  (export
    display-condition)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries ports))

  (define/who display-condition
    (case-lambda
      [(obj port)
       (unless (textual-output-port? port)
         (assertion-violation who "invalid port argument" port))
       (display (format "Exception occured with value ~a" obj) port)]
      [(obj)
       (display-condition obj (current-output-port))]))

  )
