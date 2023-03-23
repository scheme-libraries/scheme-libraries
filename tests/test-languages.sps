#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries languages)
        (scheme-libraries testing))

(define-language Lsrc
  (terminals
   (symbol (x)))
  (Expr (e)
    x))

(test-begin "languages")

(test-equal '(define-language Lsrc (entry Expr) (terminals (symbol (x))) (Expr (e) x))
  (let ()
    (define-language Lsrc
      (terminals
        (symbol (x)))
      (Expr (e)
        x))
    (language->datum Lsrc)))

(test-equal '(define-language L1
               (entry Expr)
               (terminals (symbol (x)) (symbol (y)))
               (Expr (e f) y))
  (let ()
    (define-language Lsrc
      (terminals
        (symbol (x)))
      (Expr (e)
        x)
      (Stmt (s)
        x))
    (define-language L1
      (extends Lsrc)
      (terminals
        (+ (symbol (y))))
      (Expr (e f)
        (+ y)
        (- x))
      (Stmt ()
        (- x)))

    (language->datum L1)))

(test-end "languages")
