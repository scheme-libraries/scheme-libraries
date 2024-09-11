#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2024).

(library (scheme-libraries multiple-values)
  (export
    define-values-map
    define-values-append-map
    define-values-fold-right)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries define-values)
    (scheme-libraries exceptions))

  (define-syntax/who define-values-map
    (lambda (stx)
      (syntax-case stx ()
        [(_ (x ...) proc-expr list-expr1 list-expr2 ...)
         (for-all identifier? #'(x ...))
         (with-syntax ([(t ...) (generate-temporaries #'(x ...))]
                       [(h ...) (generate-temporaries #'(x ...))]
                       [(nil ...) (map (lambda (x) #''()) #'(x ...))]
                       [(ls1 ls2 ...) (generate-temporaries #'(list-expr1 list-expr2 ...))]
                       [who #''define-values-map])
           #'(define-values (x ...)
               (let ([proc proc-expr]
                     [ls1 list-expr1]
                     [ls2 list-expr2]
                     ...)
                 (unless (procedure? proc)
                   (nonprocedure-violation who proc))
                 (let ([n (list-length ls1 who)])
                   (unless (fx=? (list-length ls2 who) n)
                     (length-violation who ls1 ls2))
                   ...
                   (let f ([ls1 ls1] [ls2 ls2] ...)
                     (if (null? ls1)
                         (values nil ...)
                         (let-values ([(t ...) (f (cdr ls1) (cdr ls2) ...)]
                                      [(h ...) (proc (car ls1) (car ls2) ...)])
                           (values (cons h t) ...))))))))]
        [_ (syntax-violation who "invalid syntax" stx)])))

  (define-syntax/who define-values-append-map
    (lambda (stx)
      (syntax-case stx ()
        [(_ (x ...) proc-expr list-expr1 list-expr2 ...)
         (for-all identifier? #'(x ...))
         (with-syntax ([(t ...) (generate-temporaries #'(x ...))]
                       [(h ...) (generate-temporaries #'(x ...))]
                       [(nil ...) (map (lambda (x) #''()) #'(x ...))]
                       [(ls1 ls2 ...) (generate-temporaries #'(list-expr1 list-expr2 ...))]
                       [who #''define-values-map])
           #'(define-values (x ...)
               (let ([proc proc-expr]
                     [ls1 list-expr1]
                     [ls2 list-expr2]
                     ...)
                 (unless (procedure? proc)
                   (nonprocedure-violation who proc))
                 (let ([n (list-length ls1 who)])
                   (unless (fx=? (list-length ls2 who) n)
                     (length-violation who ls1 ls2))
                   ...
                   (let f ([ls1 ls1] [ls2 ls2] ...)
                     (if (null? ls1)
                         (values nil ...)
                         (let-values ([(t ...) (f (cdr ls1) (cdr ls2) ...)]
                                      [(h ...) (proc (car ls1) (car ls2) ...)])
                           (values (append h t) ...))))))))]
        [_ (syntax-violation who "invalid syntax" stx)])))

  (define-syntax/who define-values-fold-right
    (lambda (stx)
      (syntax-case stx ()
        [(_ (x ...) proc-expr (e ...) list-expr1 list-expr2 ...)
         (and (for-all identifier? #'(x ...))
              (fx=? (length #'(x ...)) (length #'(e ...))))
         (with-syntax ([(t ...) (generate-temporaries #'(x ...))]
                       [(ls1 ls2 ...) (generate-temporaries #'(list-expr1 list-expr2 ...))]
                       [who #''define-values-fold-right])
           #'(define-values (x ...)
               (let ([proc proc-expr]
                     [ls1 list-expr1]
                     [ls2 list-expr2]
                     ...)
                 (unless (procedure? proc)
                   (nonprocedure-violation who proc))
                 (let ([n (list-length ls1 who)])
                   (unless (fx=? (list-length ls2 who) n)
                     (length-violation who ls1 ls2))
                   ...
                   (let f ([ls1 ls1] [ls2 ls2] ...)
                     (if (null? ls1)
                         (values e ...)
                         (let-values ([(t ...) (f (cdr ls1) (cdr ls2) ...)])
                           (proc (car ls1) (car ls2) ... t ...))))))))]
        [_ (syntax-violation who "invalid syntax" stx)])))

  (define length-violation
    (lambda (who ls1 ls2)
      (assertion-violationf who "lists ~s and ~s differ in length" ls1 ls2)))

  (define list-length
    (lambda (ls who)
      (guard (c [(assertion-violation? c)
                 (assertion-violationf who "~s is not a list" ls)])
        (length ls))))

  (define nonprocedure-violation
    (lambda (who obj)
      (assertion-violationf who "~s is not a procedure" obj))))
