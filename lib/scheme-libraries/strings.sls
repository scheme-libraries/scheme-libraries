#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries strings)
  (export
    string-split
    string-prefix?)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define/who string-split
    (lambda (s)
      (unless (string? s)
        (assertion-violation who "invalid string argument" s))
      (let ([n (string-length s)])
        (let f ([i 0])
          (cond
           [(= i n)
            '()]
           [(char-whitespace? (string-ref s i))
            (f (+ i 1))]
           [else
            (let g ([j (+ i 1)])
              (cond
               [(= j n)
                (list (substring s i j))]
               [(char-whitespace? (string-ref s j))
                (cons (substring s i j)
                      (f (+ j 1)))]
               [else
                (g (+ j 1))]))])))))

  (define/who string-prefix?
    (lambda (s1 s2)
      (unless (string? s1)
        (assertion-violation who "invalid prefix string argument" s1))
      (unless (string? s2)
        (assertion-violation who "invalid string argument" s2))
      (let ([n (string-length s1)])
        (and (>= (string-length s2) n)
             (let f ([i 0])
               (or (= i n)
                   (and (char=? (string-ref s1 i)
                                (string-ref s2 i))
                        (f (+ i 1)))))))))

  )
