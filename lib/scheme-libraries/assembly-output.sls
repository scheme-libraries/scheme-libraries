#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries assembly-output)
  (export
    assembly-output-port
    emit
    emit-comment
    emit-label)
  (import
    (rnrs)
    (scheme-libraries assembly-output $target)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries ports)
    (scheme-libraries thread-parameters))

  (define/who emit-comment
    (lambda (comment)
      (unless (string? comment)
        (assertion-violation who "invalid comment argument" comment))
      (let ([n (string-length comment)]
            [port (assembly-output-port)])
        (put-string port "/* ")
        (let f ([i 0] [prev-ch #f])
          (unless (= i n)
            (let ([ch (string-ref comment i)])
              (define emit-escape
                (lambda ()
                  (put-char port #\\)))
              (case ch
                [(#\*)
                 (when (eqv? prev-ch #\/)
                   (emit-escape))]
                [(#\/)
                 (when (eqv? prev-ch #\*)
                   (emit-escape))])
              (put-char port ch)
              (f (+ i 1) ch))))
        (put-string port " */\n"))))

  (define emit-label
    (lambda (label)
      (put-string (assembly-output-port)
                  (format "~s:~%"
                          (label->string label)))))

  (define/who emit
    (lambda (key . arg*)
      (unless (symbol? key)
        (assertion-violation who "invalid key argument" key))
      (let ([port (assembly-output-port)])
        (put-string port "\t")
        (put-string port (symbol->string key))
        (do ([sep "\t" ", "]
             [arg* arg* (cdr arg*)])
            ((null? arg*))
          (put-string port sep)
          (put-string port (operand->string (car arg*))))
        (put-string port "\n"))))

  (define/who assembly-output-port
    (make-thread-parameter
     (current-output-port)
     (lambda (port)
       (unless (textual-output-port? port)
         (assertion-violation who "invalid port argument" port))
       port)))
  )
