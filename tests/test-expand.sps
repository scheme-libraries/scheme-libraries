#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  ;; XXX
  (only (chezscheme) trace-define)
  (scheme-libraries testing)
  (scheme-libraries reading annotated-datums)
  (scheme-libraries syntax core-environment)
  (scheme-libraries syntax expressions)
  (scheme-libraries syntax expand)
  (scheme-libraries syntax syntax-objects))

(define expand-datum
  (lambda (x)
    (expand (datum->annotated-datum x)
            (core-environment))))

(define-syntax test-expand
  (syntax-rules ()
    [(test-expand expected source)
     (test-assert (expression=? `expected (expand-datum `source)))]))

(test-begin "expand")

(test-expand (quote 10) 10)

(test-expand (lambda (x.0)
               (letrec* ()
                 x.0))
  (lambda (x) x))

(test-expand (lambda (x.0)
               (letrec* ([y.1 x.0])
                 y.1))
  (lambda (x)
    (define y x)
    y))

(test-expand (void)
  (void))

(test-expand (lambda ()
               (letrec* ([a.0 (void)])
                 '1))
  (lambda ()
    (define a)
    1))

(test-end "expand")
