#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries reading annotated-datums)
  (scheme-libraries syntax bootstrap-environment)
  (scheme-libraries syntax expressions)
  (scheme-libraries syntax expand)
;;  (scheme-libraries syntax libraries)
  (scheme-libraries syntax syntax-objects)
  (scheme-libraries syntax eval)
  #;(rnrs eval)

  )

(display "===========================\n")

;;(eval '(let-syntax ((foo (lambda (stx) #f))) foo) (environment '(rnrs)))

(define expand-datum
  (lambda (x)
    (expand (datum->annotated-datum x)
            (bootstrap-environment))))

;;; Bei (rnrs eval): ruft den eingebauten expander ein...

#;
(display "===========================\n")

(eval '3 (environment))

#;
(expand-datum
 '(lambda ()
    (define-syntax foo
      (lambda (stx)
        #f))
    foo))
