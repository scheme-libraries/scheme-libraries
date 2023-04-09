#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries numbers)
  (export
    exact-integer?
    exact-positive-integer?
    exact-nonnegative-integer?
    nonnegative-fixnum?
    int32?
    int64?
    ->int64
    ->uint64)
  (import
    (rnrs)
    (scheme-libraries define-who))

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
           (not (fxnegative? obj)))))

  (define int32?
    (lambda (x)
      (and (exact-integer? x)
           (<= (- (expt 2 31)) x (- (expt 2 31) 1)))))

  (define int64?
    (lambda (x)
      (and (exact-integer? x)
           (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))

  (define/who ->uint64
    (lambda (x)
      (unless (exact-integer? x)
        (assertion-violation who "invalid number argument" x))
      (bitwise-bit-field x 0 64)))

  (define/who ->int64
    (lambda (x)
      (unless (exact-integer? x)
        (assertion-violation who "invalid number argument" x))
      (mod0 x (expt 2 64))))

  )
