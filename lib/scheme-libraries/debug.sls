#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries debug)
  (export
    debug
    trace
    current-trace
    print-trace)
  (import
    (rnrs)
    (scheme-libraries with-implicit)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries info)
    (scheme-libraries system)
    (scheme-libraries parameters)
    (scheme-libraries thread-parameters))

  (define-syntax/who debug
    (lambda (stx)
      (syntax-case stx ()
        [(debug . form)
         ;; TODO: Implement and check debug level.
         #'form]
        [_ (syntax-violation who "invalid syntax" stx)])))

  (define current-trace
    (begin
      (debug info "Initializing current-trace")
      (make-thread-parameter '())))

  #;
  (define-syntax trace
    (syntax-rules ()
      [(trace e1 b1 b2 ...)
       (parameterize ([current-trace (cons e (current-trace))])
         b1 b2 ...)]))

  (define-syntax/who trace
    (lambda (stx)
      (syntax-case stx (define define/who lambda)
        [(_ . (define id (lambda formals body1 ... body2)))
         (identifier? #'id)
         #'(define id
             (lambda arg*
               (let ([x (cons `(id ,@arg*) (current-trace))])
                 #;
                 (when (> (length x) 8)
                   (display "Foo!\n"))
                 (parameterize ([current-trace x
                                               #;
                                               (cons `(id ,@arg*)
                                               (current-trace))])
                   (apply (lambda formals
                            body1 ... body2)
                          arg*)))))]
        [(k . (define/who id e ... (lambda formals body1 ... body2)))
         (identifier? #'id)
         (with-implicit (k who)
           #'(define id
               (let ([who 'id])
                 (lambda arg*
                   e ...
                   (let ([x (cons `(id ,@arg*) (current-trace))])
                     (parameterize ([current-trace x
                                                   #;
                                                   (cons `(id ,@arg*)
                                                   (current-trace))])
                       (apply (lambda formals
                                body1 ... body2)
                              arg*)))))))]
        [_ (syntax-violation who "invalid syntax" stx)])))

  (trace define print-trace
    (lambda ()
      (define p (current-error-port))
      (for-each
        (lambda (trace)
          (display (format "[trace][~s]: ~s~%" (system) trace) p))
        (current-trace))))

  )
