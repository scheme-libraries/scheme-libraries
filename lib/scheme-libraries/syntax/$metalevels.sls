#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $metalevels)
  (export
    metalevel?
    metalevel:syntax
    metalevel:run
    metalevel-for-syntax
    current-metalevel
    current-metalevel-for-syntax)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries numbers)
    (scheme-libraries parameters)
    (scheme-libraries thread-parameters))

  ;; Metalevels

  (define metalevel?
    (lambda (obj)
      (exact-integer? obj)))

  (define metalevel-for-syntax
    (lambda (ml)
      (fxnot ml)))

  (define metalevel:syntax
    (lambda () -1))

  (define metalevel:run
    (lambda () 0))

  (define/who current-metalevel
    (make-parameter (metalevel:run)
      (lambda (x)
        (unless (metalevel? x)
          (assertion-violation who "invalid metalevel" x))
        x)))

  (define current-metalevel-for-syntax
    (lambda ()
      (metalevel-for-syntax (current-metalevel))))

  )
