#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries counters)
  (export
    make-counter)
  (import
    (rnrs)
    (scheme-libraries impure))

  (define make-counter
    (lambda ()
      (let ([counter 0])
        (lambda ()
          (let ([count counter])
            (increment! counter)
            count))))))
