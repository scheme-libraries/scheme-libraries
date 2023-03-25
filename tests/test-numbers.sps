#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries numbers)
        (scheme-libraries testing))

(test-begin "numbers")

(test-assert (not (int32? "0")))
(test-assert (not (int32? "0.0")))
(test-assert (int32? 1))
(test-assert (int32? (- (expt 2 31))))
(test-assert (not (int32? (- 0 (expt 2 31) 1))))
(test-assert (int32? (- (expt 2 31) 1)))
(test-assert (not (int32? (expt 2 31))))

(test-assert (not (int64? "0")))
(test-assert (not (int64? "0.0")))
(test-assert (int64? 1))
(test-assert (int64? (- (expt 2 63))))
(test-assert (not (int64? (- 0 (expt 2 63) 1))))
(test-assert (int64? (- (expt 2 63) 1)))
(test-assert (not (int64? (expt 2 63))))

(test-end "numbers")
