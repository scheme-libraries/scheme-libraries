#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions $compile-to-thunk)
  (export
    compile-to-thunk)
  (import
    (rnrs)
    (rnrs eval)
    (scheme-libraries syntax variables)
    (scheme-libraries atoms)
    (scheme-libraries impure)
    (scheme-libraries match))

  (define compile-to-thunk
    (lambda (e)
      (let-values ([(e vals) (parse e)])

        ;; FIXME
        ;;(display "Eval: ")
        ;;(display e) (newline)

        ((eval `(case-lambda [(vals) (case-lambda [() ,e])]) (runtime-environment))
         vals))))

  (define parse
    ;; TODO: Write a full expression parser to catch syntactic
    ;; mistakes.
    (lambda (e)
      (define n 0)
      (define val* '())
      (define out
        (let f ([e e])
          (match e
            [',x
             (guard (not (atom? x)))
             (let ([e `(vector-ref vals ,n)])
               (increment! n)
               (prepend! val* (list x))
               e)]
            [(,[e1] . ,[e2])
             `(,e1 . ,e2)]
            [#(,[e*] ...)
             `#(,e* ...)]
            [,x
             (guard (atom? x))
             x]
            [,x
             (guard (variable? x))
             (variable-name x)])))
      (values out
              (list->vector (reverse val*)))))

  (define runtime-environment
    (let ([env #f])
      (lambda ()
        (or env
            (begin
              (set! env (environment '(scheme-libraries syntax expressions $runtime)))
              env))))))
