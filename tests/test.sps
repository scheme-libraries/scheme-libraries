(import (rnrs)
        (scheme-libraries reading annotated-datums)
        (scheme-libraries syntax bootstrap-environment)
        (scheme-libraries syntax expand))

(define expr
  '(lambda ()
     (define-syntax foo
       (lambda (stx)
         #f))
     foo))

(expand (datum->annotated-datum expr)
  (bootstrap-environment))
