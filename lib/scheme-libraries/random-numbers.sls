#!r6rs

(library (scheme-libraries random-numbers)
  (export
    random
    random-seed)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries numbers)
    (scheme-libraries thread-parameters))

  (define/who random-seed
    (make-thread-parameter 4294967295
      (lambda (x)
        (unless (and (exact-integer? x)
                     (< 0 x (expt 2 32)))
          (assertion-violation who "invalid argument" x))
        x)))

  (define/who random
    (lambda (x)
      (unless (and (if (inexact? x)
                       (and (real? x))
                       (and (integer? x)))
                   (positive? x))
        (assertion-violation who "invalid argument" x))
      (let ([seed (bitwise-bit-field (+ (* 1103515245 (random-seed))
                                        12345)
                                     0 31)])
        (random-seed seed)
        (if (exact? x)
            (mod seed x)
            (fl/ (fl* (real->flonum x) (real->flonum seed))
                 (flexpt 2.0 31.0)))))))
