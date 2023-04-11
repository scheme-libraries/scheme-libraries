#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries union-find)
  (export
    union-find)
  (import
    (rnrs)
    (scheme-libraries boxes)
    (scheme-libraries define-who))

  ;; For this particular implementation, see Michael D. Adams and
  ;; R. Kent Dybvig: Efficient Nondestructive Equality Checking for
  ;; Trees and Graphs.

  (define/who union-find
    (define find
      (lambda (b)
        (let ([n (unbox b)])
          (if (box? n)
              (let f ([b b] [n n])
                (let ([m (unbox n)])
                  (cond
                   [(box? m)
                    (set-box! b m)
                    (f n m)]
                   [else n])))
              b))))
    (lambda (ht x y)
      (unless (hashtable? ht)
        (assertion-violation who "invalid hashtable argument" ht))
      (let ([bx (hashtable-ref ht x #f)]
            [by (hashtable-ref ht y #f)])
        (if (not bx)
            (if (not by)
                (let ([b (box 1)])
                  (hashtable-set! ht x b)
                  (hashtable-set! ht y b)
                  #f)
                (let ([ry (find by)])
                  (hashtable-set! ht x ry)
                  #f))
            (if (not by)
                (let ([rx (find bx)])
                  (hashtable-set! ht y rx)
                  #f)
                (let ([rx (find bx)]
                      [ry (find by)])
                  (or (eq? rx ry)
                      (let ([nx (unbox rx)]
                            [ny (unbox ry)])
                        (let ([n (fx+ nx ny)])
                          (cond
                           [(fx>? nx ny)
                            (set-box! ry rx)
                            (set-box! rx n)]
                           [else
                            (set-box! rx ry)
                            (set-box! ry n)]))
                        #f))))))))




  )
