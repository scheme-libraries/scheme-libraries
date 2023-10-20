#!r6rs

(library (scheme-libraries random-numbers)
  (export
    random
    random-seed)
  (import
    (rnrs)
    (scheme-libraries $config)
    (scheme-libraries define-who)
    (scheme-libraries numbers)
    (scheme-libraries thread-parameters))

  ;; TODO: Replace by MRG32k3a random generator (see SRFI 27).

  (define getrandom
    (lambda ()
      (call-with-port
       (open-file-input-port random-source (file-options) (buffer-mode none))
       (lambda (port)
         (let ([x (bitwise-bit-field (bytevector-uint-ref (get-bytevector-n port 4)
                                                          0
                                                          (endianness little)
                                                          4)
                                     0 31)])
           (if (zero? x)
               4294967295
               x))))))

  (define/who random-seed
    (make-thread-parameter (getrandom)
      (lambda (x)
        (unless (and (exact-integer? x)
                     (< 0 x (expt 2 32)))
          (assertion-violation who "invalid argument" x))
        x)))

  (define/who random
    (lambda (x)
      (unless (and (if (inexact? x)
                       (and (real? x))
                       (and (integer? x)))
                   (positive? x))
        (assertion-violation who "invalid argument" x))
      (let ([seed (bitwise-bit-field (+ (* 1103515245 (random-seed))
                                        12345)
                                     0 31)])
        (random-seed seed)
        (let ([r (bitwise-bit-field seed 16 31)])
          (cond
           [(exact? x)
            (when (> x 32767)
              (raise (condition (make-implementation-restriction-violation)
                       (make-who-condition who)
                       (make-message-condition "range too large")
                       (make-irritants-condition (list x)))))
            (mod r x)]
           [(inexact? x)
            (fl/ (fl* (real->flonum x) (real->flonum r))
                 (flexpt 2.0 15.0))]
           [else (assert #f)])))))

  )
