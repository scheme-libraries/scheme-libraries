#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries uuid)
  (export
    random-uuid
    uuid->string
    uid)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-values)
    (scheme-libraries define-who)
    (scheme-libraries random-numbers))

  (define random-uuid
    (lambda ()
      (define uuid (make-bytevector 16))
      (do ((i 0 (+ i 1)))
          ((= i 16) uuid)
        (bytevector-u8-set! uuid
                            i
                            (case i
                              ((6)
                               (+ #x40 (random #x10)))
                              ((8)
                               (+ #x80 (random #x40)))
                              (else
                               (random #x100)))))))

  (define/who (uuid->string uuid)
    (unless (and (bytevector? uuid)
                 (= (bytevector-length uuid) 16))
      (assertion-violation who "invalid uuid argument" uuid))
    (apply format "~a~a~a~a-~a~a-~a~a-~a~a-~a~a~a~a~a~a"
           (let f ([i 0])
             (if (fx=? i 16)
                 '()
                 (let ([u8 (bytevector-u8-ref uuid i)])
                   (define-values (high low) (div-and-mod u8 16))
                   (cons (string (hex-digit high) (hex-digit low))
                         (f (fx+ i 1))))))))


  (define/who uid
    (lambda (prefix)
      (unless (symbol? prefix)
        (assertion-violation who "invalid prefix argument" prefix))
      (string->symbol (format "~a-~a" (symbol->string prefix) (uuid->string (random-uuid))))))

  (define (hex-digit n)
    (integer->char (+ n (if (<= 0 n 9) #x30 #x57)))))
