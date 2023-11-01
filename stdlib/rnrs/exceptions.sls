#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs exceptions (6))
  (export
    guard
    =>
    else
    with-exception-handler
    raise
    raise-continuable)
  (import
    ($system)
    (scheme-libraries define-who))

  (define-syntax/who guard
    (lambda (x)
      (syntax-case x (else)
        [(guard (c cl ... [else e1 ... e2])
           b1 ... b2)
         (identifier? #'c)
         #'((call/cc
             (lambda (k)
               (with-exception-handler
                   (lambda (c)
                     (k (lambda ()
                          (cond cl ... [else e1 ... e2]))))
                 (lambda ()
                   (call-with-values (lambda () b1 ... b2)
                     (case-lambda
                       [(val) (lambda () val)]
                       [val* (lambda () (apply values val*))])))))))]
        [(guard (c cl1 ... cl2)
           b1 ... b2)
         (identifier? #'c)
         #'((call/cc
             (lambda (kouter)
               (with-exception-handler
                   (lambda (obj)
                     ((call/cc
                       (lambda (khandler)
                         (kouter
                          (lambda ()
                            (let ([c obj])
                              (cond
                               cl1 ... cl2
                               [else
                                (khandler
                                 (lambda ()
                                   (raise-continuable obj)))]))))))))
                 (lambda ()
                   (call-with-values (lambda () b1 ... b2)
                     (case-lambda
                       [(val) (lambda () val)]
                       [val* (lambda () (apply values val*))])))))))]
        [_ (syntax-violation who "invalid syntax" x)]))))
