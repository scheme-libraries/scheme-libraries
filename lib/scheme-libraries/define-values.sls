#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries define-values)
  (export
    define-values)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define-syntax/who define-values
    (lambda (x)
      (define parse-formals
        (lambda (formals)
          (define output
            (lambda (id*)
              (let f ([id* id*] [i 0])
                (if (null? id*)
                    '()
                    `((,(car id*) ,i)
                      ,@(f (cdr id*) (+ i 1)))))))
          (syntax-case formals ()
            [(id ...)
             (for-all identifier? #'(id ...))
             (output #'(id ...))]
            [(id1 ... . id2)
             (for-all identifier? #'(id1 ... id2))
             (output #'(id1 ... id2))]
            [_
             (syntax-violation who "invalid formals" x formals)])))
      (syntax-case x ()
        [(_ () expr)
         #'(define tmp (begin expr #f))]
        [(_ (id) expr)
         (identifier? #'id)
         #'(define id expr)]
        [(_ id expr)
         (identifier? #'id)
         #'(define id (let-values ([tmp expr]) tmp))]
        [(_ formals expr)
         (with-syntax ([((id i) ...) (parse-formals #'formals)])
           #'(begin
               (define tmp (let-values ([formals expr])
                             (vector id ...)))
	       (define id (vector-ref tmp i))
	       ...))]
        [_
         (syntax-violation who "invalid syntax" x)]))))
