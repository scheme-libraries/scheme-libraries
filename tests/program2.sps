#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs base)
  (rnrs bytevectors))

(assert (symbol=? 'little (endianness little)))
(assert (symbol=? 'big (endianness big)))
(assert (symbol=? (native-endianness)
                  (let-syntax ([m (lambda (stx)
                                    #`(endianness #,(datum->syntax #'here (native-endianness))))])
                    m)))
