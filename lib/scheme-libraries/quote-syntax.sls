#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries quote-syntax)
  (export
    quote-syntax)
  (import
    (rnrs))

  (define-syntax quote-syntax
    (lambda (stx)
      (syntax-case stx ()
        [(_ id)
         (identifier? #'id)
         (with-syntax ([(tmp) (generate-temporaries '(id))])
           #`(datum->syntax #'#,(datum->syntax #'id (syntax->datum #'tmp)) 'id))]))))
