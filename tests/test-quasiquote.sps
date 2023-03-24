#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries match)
        (scheme-libraries testing)
        (scheme-libraries with-implicit))

(define-syntax test-quasiquote
  (lambda (x)
    (syntax-case x ()
      [(k actual expected)
       (with-implicit (k match)
         #'(test-equal actual
             (match #f [,_ expected])))])))

(test-begin "quasiquote")

(test-quasiquote '(list 3 4)
                 `(list ,(+ 1 2) 4))

(test-quasiquote '(a 3 4 5 6 b)
                 `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))

(test-quasiquote '(a 3 4 5 6 b)
                 `(a ,(+ 1 2) ,(map abs '(4 -5 6)) ... b))

(test-quasiquote '((1 . a) (2 . b) (3 . c))
                 `((,'(1 2 3) . ,'(a b c)) ...))

(test-quasiquote '(((a x) (a 1)) ((a x) (a 2)) ((a x) (a 3)))
                 `(((a ,'((x 1) (x 2) (x 3))) ...) ...))

(test-quasiquote '((a x) (a 1) (a x) (a 2) (a x) (a 3))
                 `((a ,'((x 1) (x 2) (x 3))) ... ...))

(test-quasiquote '((a x 1) (a x 2) (a x 3))
                 `((a ,@'((x 1) (x 2) (x 3))) ...))

(test-quasiquote '((1 2 3) ...)
                 `(... (,'(1 2 3) ...)))

(test-quasiquote '(a `(b ,(list 1 2) ... ,(foo 1 3 d) e) f)
                 `(a `(b ,(list 1 2) ... ,(foo ,(list 1 3) ... d) e) f))

(test-quasiquote '(a 3)
                 `((unquote 'a (+ 1 2))))

(test-quasiquote '(a b c d e f)
                 `((unquote-splicing '(a b c) '(d e f))))

(test-quasiquote '((a x 1) (a x 2) (a x 3))
                 `((a ,'((x 1) (x 2) (x 3)) ...) ...))

(test-quasiquote '(1 2 3 4 5 6)
		 `((unquote (list 1 2 3) (list 4 5 6)) ...))

(test-quasiquote '(x 1 x 2 x 3)
		 `(,@'((x 1) (x 2) (x 3)) ...))

(test-end "quasiquote")
