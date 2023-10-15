#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries void)
  (export
    void)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define/who void
    (define-record-type void
      (nongenerative void-e509d690-c552-4ca5-b05b-dde166eb3b56)
      (opaque #t) (sealed #t))
    (let ([void (make-void)])
      (lambda ()
        void))))
