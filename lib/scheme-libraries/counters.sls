#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries counters)
  (export
    make-counter)
  (import
    (rnrs)
    (scheme-libraries numbers)
    (scheme-libraries parameters))

  (define make-counter
    (lambda ()
      (let ([counter
             (make-parameter 0
                             (lambda (x)
                               (unless (exact-nonnegative-integer? x)
                                 (assertion-violation #f "invalid counter argument" x))
                               x))])
        (values (lambda ()
                  (let ([count (counter)])
                    (counter (+ count 1))
                    count))
                counter)))))
