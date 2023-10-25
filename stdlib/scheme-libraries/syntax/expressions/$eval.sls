#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions $eval)
  (export
    $eval)
  (import
    (prefix ($system) system:))

  (define $eval
    (lambda (exp)
      ((system:$eval `(lambda (default-stdlibs-collection-datum) ,exp))
       default-stdlibs-collection-datum))))
