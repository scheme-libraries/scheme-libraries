#!r6rs

(library (scheme-libraries repl)
  (export
    (rename (waiter-prompt-string repl-prompt-string))
    console-input-port
    console-output-port
    repl)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries parameters)
    (only (chezscheme)
      abort
      console-input-port
      console-output-port
      new-cafe
      waiter-prompt-string))

  (define/who repl
    (lambda (eval-proc)
      (unless (procedure? eval-proc)
        (assertion-violation who "invalid eval procedure argument" eval-proc))
      (call/cc
       (lambda (c)
         ((call/cc
           (lambda (k)
             (new-cafe (lambda (x)
                         (with-exception-handler
                             (lambda (con)
                               (call/cc
                                (lambda (c)
                                  (k (lambda ()
                                       (call-with-values
                                           (lambda ()
                                             (raise-continuable con))
                                         c))))))
                           (lambda ()
                             (eval-proc x)))))
             (c)))))))))
