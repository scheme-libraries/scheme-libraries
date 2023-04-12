#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries gensyms)
  (export
    gensym
    gensym?
    gensym-count
    gensym-prefix
    gensym-suffix
    gensym-marker)
  (import
    (rnrs)
    (scheme-libraries counters)
    (scheme-libraries define-values)
    (scheme-libraries define-who))

  (define-values (counter gensym-count)
    (make-counter))

  (define/who gensym
    (case-lambda
      [(prefix separator)
       (unless (string? prefix)
         (assertion-violation who "invalid prefix argument" prefix))
       (unless (string? separator)
         (assertion-violation who "invalid separator argument" separator))
       (string->symbol
        (string-append prefix separator (number->string (counter))))]
      [(prefix) (gensym prefix "")]
      [() (gensym "g")]))

  (define/who gensym-prefix
    (lambda (sym)
      (unless (symbol? sym)
        (assertion-violation who "invalid gensym argument" sym))
      (let* ([s (symbol->string sym)]
             [n (string-length s)])
        (let f ([k (fx- n 1)])
          (if (and (fx>=? k 0)
                   (char<=? #\0 (string-ref s k) #\9))
              (f (fx- k 1))
              (substring s 0 (fx+ k 1)))))))

  (define/who gensym-suffix
    (lambda (sym)
      (unless (symbol? sym)
        (assertion-violation who "invalid gensym argument" sym))
      (let* ([s (symbol->string sym)]
             [n (string-length s)])
        (let f ([k (fx- n 1)])
          (if (and (fx>=? k 0)
                   (char<=? #\0 (string-ref s k) #\9))
              (f (fx- k 1))
              (let ([k (fx+ k 1)])
                (and (fx<? k n)
                     (substring s k n))))))))

  (define/who gensym?
    (lambda (x)
      (unless (symbol? x)
        (assertion-violation who "invalid argument" x))
      (and (gensym-suffix x) #t)))

  (define/who gensym-marker
    (lambda (sym)
      (unless (symbol? sym)
        (assertion-violation who "invalid gensym argument" sym))
      (let* ([s (symbol->string sym)]
             [n (string-length s)])
        (let f ([k (fx- n 1)])
          (and (not (fxnegative? k))
               (if (char<=? #\0 (string-ref s k) #\9)
                   (f (fx- k 1))
                   (string-ref s k))))))))
