#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries handlers)
  (scheme-libraries testing))

(test-begin "handlers")

(test-equal 42
  (with-handler
      (lambda (c k)
        (assert #f))
    (lambda ()
      42)))

(test-equal 43
  (with-handler
      (lambda (c k)
        c)
    (lambda ()
      (raise 43))))

(test-equal 50
  (with-handler
      (lambda (c k)
        (k (+ c 3)))
    (lambda ()
      (+ 1 (raise-continuable 46)))))

(test-end "handlers")
