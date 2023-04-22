#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries modules)
  (export
    module)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define-syntax/who module
    (lambda (x)
      (syntax-case x ()
        [(_ (exp ...) def ...)
         (for-all identifier? #'(exp ...))
         #'(begin
             (define-syntax instantiate-module
               (syntax-rules ()
                 [(instantiate-module exp ...)
                  (begin def ...)]))
             (instantiate-module exp ...)
             (define dummy))]
        [_ (syntax-violation who "invalid syntax" x)]))))
