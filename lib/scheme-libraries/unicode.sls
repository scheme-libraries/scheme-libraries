#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries unicode)
  (export
    unicode-scalar-value?
    unicode-width)
  (import
    (rnrs)
    (scheme-libraries numbers))

  (define unicode-scalar-value?
    (lambda (obj)
      (and (exact-nonnegative-integer? obj)
           (or (<= 0 obj #xD7FF)
               (<= #xE000 #x10FFFF)))))

  (define unicode-width
    (lambda (ch)
      ;; FIXME: Return the correct Unicode width of CH.
      1)))
