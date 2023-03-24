#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries numbers)
  (export
    exact-integer?
    exact-positive-integer?
    exact-nonnegative-integer?
    nonnegative-fixnum?)
  (import
    (rnrs))

  (define exact-integer?
    (lambda (x)
      (and (integer? x)
           (exact? x))))

  (define exact-positive-integer?
    (lambda (x)
      (and (exact-integer? x)
           (positive? x))))

  (define exact-nonnegative-integer?
    (lambda (x)
      (and (exact-integer? x)
           (not (negative? x)))))

  (define nonnegative-fixnum?
    (lambda (obj)
      (and (fixnum? obj)
           (not (fxnegative? obj))))))
