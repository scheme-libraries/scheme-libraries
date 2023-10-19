#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries hashtables)
  (export
    hashtable-intern!
    hashtable-union!)
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

  (define/who hashtable-union!
    (lambda (ht1 ht2)
      (unless (hashtable? ht1)
        (assertion-violation who "invalid first hashtable argument" ht1))
      (unless (hashtable? ht2)
        (assertion-violation who "invalid second hashtable argument" ht2))
      (let-values ([(keys vals) (hashtable-entries ht2)])
        (vector-for-each
         (lambda (key val)
           (hashtable-update! ht1 key values val))
         keys vals))
      ht1)))
