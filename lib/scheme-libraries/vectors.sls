#!r6rs

(library (scheme-libraries vectors)
  (export
    vector-fold-right)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries exceptions))

  (define/who vector-fold-right
    (lambda (proc nil vec . vec*)
      (unless (procedure? proc)
        (nonprocedure-violation who proc))
      (let ([n ($vector-length vec who)])
        (for-each
         (lambda (v)
           (unless (fx=? ($vector-length v who) n)
             (length-violation who vec v)))
         vec*)
        (let ([vec* (cons vec vec*)])
          (do ([i 0 (+ i 1)]
               [res nil (apply proc
                               i
                               (fold-right (lambda (v arg*)
                                             (cons (vector-ref v i) arg*))
                                           (list res) vec*))])
              ((fx=? i n) res))))))

  (define length-violation
    (lambda (who ls1 ls2)
      (assertion-violationf who "lists ~s and ~s differ in length" ls1 ls2)))

  (define $vector-length
    (lambda (ls who)
      (guard (c [(assertion-violation? c)
                 (assertion-violationf who "~s is not a vector" ls)])
        (vector-length ls))))

  (define nonprocedure-violation
    (lambda (who obj)
      (assertion-violationf who "~s is not a procedure" obj)))

  )
