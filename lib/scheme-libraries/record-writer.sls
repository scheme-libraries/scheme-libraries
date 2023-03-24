#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries record-writer)
  (export
    record-writer)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define/who record-writer
    (lambda (rtd proc)
      (unless (record-type-descriptor? rtd)
        (assertion-violation who "invalid record type descriptor argument" rtd))
      (unless (procedure? proc)
        (assertion-violation who "invalid  procedure argument" proc))
      (values))))
