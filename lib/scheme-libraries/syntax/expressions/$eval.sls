#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions $eval)
  (export
    $eval)
  (import
    (rnrs)
    (rnrs eval)
    ;; XXX
    (scheme-libraries debug))

  (trace define $eval
    (lambda (exp)
      (eval exp (runtime-environment))))

  (define runtime-environment
    (let ([env #f])
      (lambda ()
        (or env
            (begin
              (set! env (environment '(scheme-libraries syntax expressions $runtime)))
              env))))))
