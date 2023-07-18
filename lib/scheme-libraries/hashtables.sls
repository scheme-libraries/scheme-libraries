#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries hashtables)
  (export
    hashtable-intern!)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define/who hashtable-intern!
    (lambda (ht key failure)
      (unless (hashtable? ht)
        (assertion-violation who "invalid hashtable argument" ht))
      (unless (procedure? failure)
        (assertion-violation who "invalid failure argument" failure))
      (if (hashtable-contains? ht key)
          (hashtable-ref ht key #f)
          (let ([val (failure)])
            (hashtable-set! ht key val)
            val))))
  )
