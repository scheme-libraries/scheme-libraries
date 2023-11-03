#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax current-command-line)
  (export
    current-command-line)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries parameters))

  (define/who current-command-line
    (make-parameter (command-line)
      (lambda (x)
        (unless (and (pair? x)
                     (list? x)
                     (for-all string? x))
          (assertion-violation who "invalid command line" x))
        x))))
