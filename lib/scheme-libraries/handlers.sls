#!r6rs

(library (scheme-libraries handlers)
  (export
    with-handler)
  (import
    (rnrs)
    (scheme-libraries define-who))

  ;; Interface

  (define/who with-handler
    (lambda (handler thunk)
      (unless (procedure? handler)
        (assertion-violation who "invalid handler argument" handler))
      (unless (procedure? thunk)
        (assertion-violation who "invalid thunk argument" thunk))
      (call-with-prompt (make-prompt handler) thunk)))

  ;; Implementation

  (define-record-type prompt
    (nongenerative handler-c834a0ba-d063-402a-8c67-b4d9f12302a8)
    (sealed #t) (opaque #t)
    (fields handler))

  (define mk '())

  (define base-k)

  (define underflow
    (lambda v*
      (if (pair? mk)
          (let ([k (car mk)])
            (set! mk (cdr mk))
            (cond
             [(procedure? k)
              (apply k v*)]
             [(prompt? k)
              (abort (lambda ()
                       (apply values v*)))]
             [else
              (assert #f)]))
          (apply values v*))))

  (define yield
    (lambda (obj)
      (abort-to-prompt
       (lambda (prompt handler k)
         (handler obj
                  (lambda v*
                    (call-with-prompt prompt
                        (lambda ()
                          (call-in-continuation k
                            (lambda ()
                              (apply values v*))))))))
       (lambda ()
         (raise-continuable obj)))))

  (define abort
    (call/cc
     (lambda (return)
       (call-with-values
           (lambda ()
             (with-exception-handler yield
               (lambda ()
                 (dynamic-wind
                   (lambda ()
                     )
                   (lambda ()
                     (call/cc
                      (lambda (k)
                        (set! base-k k)
                        ((call/cc return)))))
                   (lambda ()

                     )))))
         underflow))))

  (define call-with-prompt
    (lambda (prompt producer)
      (call-with-metacontinuation mk
        (lambda (subk)
          (set! mk (cons prompt subk))
          (abort
           producer)))))

  (define call-with-metacontinuation
    (lambda (base proc)
      (call/cc
       (lambda (k)
         (proc
          (if (eqv? base-k k)
              base
              (cons k base)))))))

  (define abort-to-prompt
    (lambda (f fail)
      (call-with-composable-continuation
        (lambda (subk prompt handler new-mk)
          (set! mk new-mk)
          (call-with-metacontinuation subk
            (lambda (subk)
              (abort (lambda () (f prompt handler subk))))))
        fail)))

  (define call-in-continuation
    (lambda (subk th)
      (call-with-metacontinuation mk
       (lambda (k)
         (set! mk (append subk k))
         (abort th)))))

  (define call-with-composable-continuation
    (lambda (c fail)
      (let f ([mk mk] [c c])
        (if (null? mk)
            (fail)
            (let ([k (car mk)]
                  [mk (cdr mk)])
              (if (prompt? k)
                  (c '() k (prompt-handler k) mk)
                  (f mk
                     (lambda (subk prompt proc new-mk)
                       (c (cons k subk)
                          prompt
                          proc
                          new-mk))))))))))
