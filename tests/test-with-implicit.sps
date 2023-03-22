#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries with-implicit)
        (scheme-libraries testing))

(test-begin "with-implicit")

(test-equal 'inner
  (let ([n 'outer])
    (let-syntax ([m (lambda (x)
                      (syntax-case x ()
                        [(k e)
                         (with-implicit (k n)
                           #'(let ([n 'inner])
                               e))]))])
      (m n))))

(test-end "with-implicit")
