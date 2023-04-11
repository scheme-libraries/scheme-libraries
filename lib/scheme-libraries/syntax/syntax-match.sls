#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax syntax-match)
  (export
    syntax-match
    syntax-extend-backquote
    unquote
    ...
    _
    ->
    guard)
  (import
    (rnrs)
    (scheme-libraries define-match)
    (scheme-libraries syntax syntax-objects)
    (rename (scheme-libraries syntax syntax-objects)
            (syntax-car match-car)
            (syntax-cdr match-cdr)
            (syntax-pair? match-pair?)
            (syntax-null? match-null?)
            (syntax-vector? match-vector?)
            (syntax-vector->list match-syntax-vector->list))
    ;; FIXME: Eventually remove this.
    (scheme-libraries reading annotated-datums))

  (define syntax-equal?
    (lambda (x y)
      (let f ([x x] [y y])
        (let-values ([(x y)
                      (if (syntax-object? y)
                          (values y x)
                          (values x y))])
          ;; XXX

          ))

      )

    )

  (define-syntax syntax-quote
    (lambda (x)
      (syntax-case x ()
        [(_ e)
         (let f ([e e])
           (syntax-case e ()
             [(e1 . e2)
              #`(cons (f #,e1) (f #,e2))]
             [#(e ...)
              (vector #,(map f #'(e ...)))]
             [x
              (identifier? #'x)
              ;; FIXME: Wrap it in substitutions for core library.
              (annotated-datum->syntax-object (make-annotated-datum x #f))]
             [e (syntax->datum #'e)]))])))

  (define-match (syntax-match syntax-extend-backquote)))
