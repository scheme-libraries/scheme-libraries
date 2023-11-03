#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs base)
  (rnrs bytevectors)
  (rnrs records syntactic))

;;; Endianness
(assert (symbol=? 'little (endianness little)))
(assert (symbol=? 'big (endianness big)))
(assert
 (symbol=? (native-endianness)
           (let-syntax
               ([m
                 (lambda (stx)
                   #`(endianness #,(datum->syntax #'here (native-endianness))))])
             m)))

;;; Records

(define-record-type foo)
(assert (foo? (make-foo)))
