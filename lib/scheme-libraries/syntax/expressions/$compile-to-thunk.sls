#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions $compile-to-thunk)
  (export
    compile-to-thunk)
  (import
    (rnrs)
    (rnrs eval))

  (define compile-to-thunk
    (lambda (e)
      (eval `(lambda () ,e) (runtime-environment))))

  (define runtime-environment
    (let ([env #f])
      (lambda ()
        (or env
            (begin
              (set! env (environment '(scheme-libraries syntax expressions $runtime)))
              env))))))
