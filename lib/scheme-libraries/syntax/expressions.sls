#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expressions)
  (export
    build
    build-begin
    build-let
    expression?
    expression=?
    compile-to-thunk
    expression->s-exp
    s-exp->expression)
  (import
    ;(prefix (chezscheme) cs:)

    (rnrs)
    (rnrs eval)
    (scheme-libraries atoms)
    (scheme-libraries define-who)
    (scheme-libraries gensyms)
    (scheme-libraries match)
    (scheme-libraries symbols)
    (scheme-libraries syntax expressions $compile-to-thunk)
    (scheme-libraries syntax variables)
    (scheme-libraries with-implicit))

  (define-syntax/who build
    (lambda (x)
      (syntax-case x ()
        [(k tmpl)
         (with-implicit (k quasiquote)
           #'(extend-backquote k `tmpl))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax/who build-begin
    (lambda (x)
      (syntax-case x ()
        [(_ e ...)
         #'(make-begin (build (e ...)))]
        [,_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax/who build-let
    (lambda (x)
      (syntax-case x ()
        [(k b e1 ... e2)
         (with-implicit (k quasiquote)
           #'(extend-backquote k
               (match `b
                 [([,x* ,e*] (... ...))
                  (build ((case-lambda [,x* ,(build-begin e1 ... e2)]) ,e* (... ...)))]
                 [,x (assertion-violation 'who "invalid bindings" x)])))]
        [,_ (syntax-violation who "invalid syntax" x)])))

  (define/who make-begin
    (define strip-begin
      (lambda (expr*)
        (match `(begin ,@expr*)
          [(begin ,[expr*] ...)
           `(,expr* ... ...)]
          [,expr (list expr)])))
    (lambda (expr*)
      (match (strip-begin expr*)
        [(,x) x]
        [(,x ,x* ...) `(begin ,x ,x* ...)])))

  (define expression?
    (lambda (x)
      (let ([ht (make-eq-hashtable)])
        (let f ([x x])
          (cond
           [(variable? x)]
           [(atom? x)]
           [(hashtable-ref ht x #f) #f]
           [(pair? x)
            (hashtable-set! ht x #t)
            (or (eq? (car x) 'quote)
                (and (f (car x)) (f (cdr x))))]
           [(vector? x)
            (hashtable-set! ht x #t)
            (let g ([k (vector-length x)])
              (or (fxzero? k)
                  (let ([k (fx- k 1)])
                    (and (f (vector-ref x k))
                         (g k)))))]
           [else
            #f])))))

  (define/who expression=?
    ;; XXX: In our tests, we use ordinary symbols for variables.
    (let ([variable?
           (lambda (x)
             (or (variable? x)
                 (and (symbol? x)
                      (eqv? (gensym-marker x) #\.))))])
      (lambda (x y)
        (unless (expression? x)
          (assertion-violation who "invalid expression argument" x))
        (unless (expression? y)
          (assertion-violation who "invalid expression argument" y))
        (let ([htx (make-eq-hashtable)]
              [hty (make-eq-hashtable)])
          (let f ([x x] [y y])
            (cond
             [(variable? x)
              (and (variable? y)
                   (cond
                    [(hashtable-ref htx x #f)
                     => (lambda (x)
                          (eq? x y))]
                    [(hashtable-ref hty y #f) #f]
                    [else
                     (hashtable-set! htx x y)
                     (hashtable-set! hty y x)
                     #t]))]
             [(pair? x)
              (and (pair? y)
                   (f (car x) (car y))
                   (f (cdr x) (cdr y)))]
             [(vector? x)
              (and (vector? y)
                   (let ([n (vector-length x)])
                     (and (fx=? n (vector-length y))
                          (let g ([k n])
                            (or (fxzero? k)
                                (let ([k (fx- k 1)])
                                  (and (f (vector-ref x k)
                                          (vector-ref y k))
                                       (g k))))))))]

             [(atom? y)
              (atom=? x y)]
             [else #f]))))))

  ;; Serialization

  (define/who expression->s-exp
    (lambda (object->s-exp e)
      (unless (procedure? object->s-exp)
        (assertion-violation who "invalid object serialize argument" object->s-exp))
      (match e
        [(begin ,[e*] ...)
         `(begin ,e* ...)]
        [(case-lambda [,(formals->s-exp -> formals*) ,[e*]] ...)
         `(case-lambda [,formals* ,e*] ...)]
        [(letrec ([,[variable->symbol -> x*] ,[e*]] ...) ,[body])
         `(letrec ([,x* ,e*] ...) ,body)]
        [(letrec* ([,[variable->symbol -> x*] ,[e*]] ...) ,[body])
         `(letrec* ([,x* ,e*] ...) ,body)]
        [(if ,[e0] ,[e1] ,[e2])
         `(if ,e0 ,e1 ,e2)]
        [(quote ,e) (guard (datum? e)) `(quote ,e)]
        [(quote ,e) `(object . ,(object->s-exp e))]
        [(set! ,[variable->symbol -> x] ,[e])
         `(set! ,x ,e)]
        [(,[e*] ...) `(,e* ...)]
        [,x (guard (symbol? x)) `(primitive ,x)]
        [,x (guard (variable? x)) (variable->symbol x)]
        [,x (assertion-violation who "invalid expression" x)])))

  (define/who s-exp->expression
    (lambda (s-exp->object e)
      (unless (procedure? s-exp->object)
        (assertion-violation who "invalid object deserialize argument" s-exp->object))
      (match e
        [(begin ,[e*] ...)
         `(begin ,e* ...)]
        [(case-lambda [,(sexp->formals -> formals*) ,[e*]] ...)
         `(case-lambda [,formals* ,e*] ...)]
        [(letrec ([,[symbol->variable -> x*] ,[e*]] ...) ,[body])
         `(letrec ([,x* ,e*] ...) ,body)]
        [(letrec* ([,[symbol->variable -> x*] ,[e*]] ...) ,[body])
         `(letrec* ([,x* ,e*] ...) ,body)]
        [(if ,[e0] ,[e1] ,[e2])
         `(if ,e0 ,e1 ,e2)]
        [(object . ,e)
         `(quote ,(s-exp->object e))]
        [(quote ,e)
         `(quote ,e)]
        [(set! ,[symbol->variable -> x] ,[e])
         `(set! ,x ,e)]
        [(primitive ,x) x]
        [(,[x*] ...) `(,x* ...)]
        [,x (guard (symbol? x)) (symbol->variable x)]
        [,x (assertion-violation who "invalid expression s-exp" x)])))

  (define sexp->formals
    (lambda (e)
      (match e
        [(,[symbol->variable -> x*] ...)
         `(,x* ...)]
        [(,[symbol->variable -> x*] ... . ,[symbol->variable -> x])
         `(,x* ... . ,x)])))

  (define formals->s-exp
    (lambda (e)
      (match e
        [(,[variable->symbol -> x*] ...)
         `(,x* ...)]
        [(,[variable->symbol -> x*] ... . ,[variable->symbol -> x])
         `(,x* ... . ,x)])))

  (define datum?
    (lambda (obj)
      (or (boolean? obj)
          (symbol? obj)
          (char? obj)
          (vector? obj)
          (null? obj)
          (pair? obj)
          (number? obj)
          (string? obj)
          (bytevector? obj))))
  )
