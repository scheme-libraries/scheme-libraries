#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries debug)
  (export
    safe-assert)
  (import
    (only (chezscheme) alias)
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries with-implicit))

  (define/who safe-assert
    (lambda (x)
      (syntax-case x ()
        [(k e1 ... e2)
         (with-syntax ([(local-assert) (generate-temporaries '(assert))])
           (with-implicit (k tmp)
             #'(let* ()
                 (alias local-assert assert)
                 (local-assert e1) ...
                 (local-assert e2))))]
        [_ (syntax-violation who "invalid syntax" x)]))))
