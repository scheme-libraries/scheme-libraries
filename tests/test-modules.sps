#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries modules))

(test-begin "modules")

(test-eqv 1
  (let* ()
    (module (x)
      (define x 1))
    1))

(test-eqv 2
  (let ([x 1])
    (module (y)
      (define x 2)
      (define y x))
    y))

(test-eqv 3
  (let ([x 1])
    (module (y)
      (define x 2)
      (define y x))
    (+ x y)))

(test-end "modules")
