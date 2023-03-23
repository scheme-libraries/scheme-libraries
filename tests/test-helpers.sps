#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries helpers)
        (scheme-libraries testing))

(test-begin "helpers")

(test-assert (symbolic-identifier=? #'x #'x))
(test-assert (not (symbolic-identifier=? #'x #'y)))
(test-assert (let-syntax ([id1 (identifier-syntax #'x)])
               (let ([id2 #'x])
                 (symbolic-identifier=? id1 id2))))

(test-end "helpers")
