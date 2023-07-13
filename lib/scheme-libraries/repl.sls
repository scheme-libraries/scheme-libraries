#!r6rs

(library (scheme-libraries repl)
  (export
    repl
    repl-prompt-string
    console-input-port
    console-output-port)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries ports)
    (scheme-libraries thread-parameters)
    (scheme-libraries void))

  (define/who repl
    (lambda (eval-proc)
      (unless (procedure? eval-proc)
        (assertion-violation who "invalid eval procedure argument" eval-proc))
      (let f ()
        (let ([e (default-prompt-and-read)])
          (unless (eof-object? e)
            (let-values ([x* (eval-proc e)])
              (for-each default-write x*))
            (f))))))

  (define default-prompt-and-read
    (lambda ()
      (let ([prompt (repl-prompt-string)])
        (display prompt (console-output-port))
        (write-char #\space (console-output-port))
        (flush-output-port (console-output-port))
        (let ([x (read (console-input-port))])
          (when (and (eof-object? x) (not (string=? prompt "")))
            (newline (console-output-port))
            (flush-output-port (console-output-port)))
          x))))

  (define/who repl-prompt-string
    (make-thread-parameter ">"
      (lambda (x)
        (unless (string? x)
          (assertion-violation who "invalid repl prompt string" x))
        x)))

  (define default-write
    (lambda (x)
      (unless (eq? x (void))
        (print x (console-output-port))
        (flush-output-port (console-output-port)))))

  (define print
    (lambda (x port)
      (write x port)
      (newline port)))

  (define console-input-port
    (make-thread-parameter (current-input-port)
      (lambda (x)
        (unless (textual-input-port? x)
          (assertion-violation "invalid textual input port" x))
        x)))

  (define console-output-port
    (make-thread-parameter (current-output-port)
      (lambda (x)
        (unless (textual-output-port? x)
          (assertion-violation "invalid textual output port" x))
        x)))


  )
