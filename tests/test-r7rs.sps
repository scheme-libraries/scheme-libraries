#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rename (rnrs)
    (syntax-rules r6rs:syntax-rules))
  (rnrs eval)
  (scheme-libraries r7rs)
  (scheme-libraries testing))

(test-begin "r7rs")

(test-group "syntax-rules"

  (test-equal '(2 3)
    (syntax->datum
     ((syntax-rules ()
        [(foo a b)
         (a b)])
      #'(a 2 3))))

  (test-equal '(2 3 4)
    (syntax->datum
     ((syntax-rules (_)
        [(foo a b)
         (a b 4)])
      #'(a 2 3))))

  (test-equal '(c 3 4)
    (syntax->datum
     ((syntax-rules (_)
        [(foo _ b)
         (a b 4)]
        [(foo a b)
         (c b 4)])
      #'(a 2 3))))

  (test-equal '(1 a b)
    (syntax->datum
     ((syntax-rules (_)
        [(foo (a _) ...)
         (0 a ...)]
        [(foo (a b) ...)
         (1 a ...)])
      #'(quux (a _) (b c)))))

  (test-equal '(1)
    (syntax->datum
     ((syntax-rules ::: ()
                    [(_ a) (a)])
      #'(_ 1))))

  (test-equal '(1 2 3)
    (syntax->datum
     ((syntax-rules ::: ()
                    [(_ a :::) (a :::)])
      #'(_ 1 2 3))))

  (test-equal '(1 2 5 3)
    (syntax->datum
     ((syntax-rules ::: ()
                    [(_ a ::: ...) (a ::: 5 ...)])
      #'(_ 1 2 3))))

  (test-equal '(1 2 (::: ...) 5 3)
    (syntax->datum
     ((syntax-rules ::: ()
                    [(_ a ::: ...) (a ::: (::: (::: ...)) 5 ...)])
      #'(_ 1 2 3))))

  (test-equal '(1 2 (::: ...) 5 3)
    (syntax->datum
     ((syntax-rules ::: ()
                    [(_ #(a ::: ...)) (a ::: (::: (::: ...)) 5 ...)])
      #'(_ #(1 2 3))))))

(test-group "syntax-error"

  (test-error #t (eval '(syntax-error "identifier expected, got" 42)
                       (environment '(rnrs) '(rnrs eval) '(scheme-libraries r7rs)))))

(test-end "r7rs")
