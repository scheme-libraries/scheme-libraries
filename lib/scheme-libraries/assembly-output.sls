#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries assembly-output)
  (export
    assembly-output-port
    emit-comment)
  (import
    (rnrs)
    (scheme-libraries assembly-output $target)
    (scheme-libraries define-who)
    (scheme-libraries thread-parameters))

  (define/who emit-comment
    (lambda (comment)
      (unless (string? comment)
        (assertion-violation who "invalid comment argument" port))
      (let ([n (string-length n)]
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

  (define/who assembly-output-port
    (make-thread-parameter
     (current-output-port)
     (lambda (port)
       (unless (textual-output-port? port)
         (assertion-violation who "invalid port argument" port))
       port)))
  )
