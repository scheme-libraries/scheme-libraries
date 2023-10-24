#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library ($condition-names)
  (export
    define-condition-name)
  (import
    ($system)
    ($record-names)
    (scheme-libraries define-who))

  (define-syntax/who define-condition-name
    (lambda (x)
      (syntax-case x ()
        [(_ condition-name rtd-expr)
         (identifier? #'condition-name)
         #'(begin
             (define rtd rtd-expr)
             (define cd
               (make-record-constructor-descriptor rtd #f #f))
             (define-record-name condition-name rtd cd))]
        [_ (syntax-violation who "invalid syntax" x)]))))
