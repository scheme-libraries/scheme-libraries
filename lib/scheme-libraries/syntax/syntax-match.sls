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
    (scheme-libraries helpers)
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

  (define match-equal?
    (lambda (x y)
      (unless (and ($identifier? x)
                   ($identifier? y))
        (assertion-violation 'syntax-match "attempt to compare non-identifier syntax objects" x y))
      ($free-identifier=? x y)))

  (define-syntax match-quote
    (lambda (x)
      (syntax-case x ()
        [(k e)
         (let f ([e #'e])
           (syntax-case e ()
             [(e1 . e2)
              #`(cons #,(f #'e1) #,(f #'e2))]
             [#(e ...)
              #`(vector #,@(map f #'(e ...)))]
             [x
              (identifier? #'x)
              (construct-name #'k "$" #'x)]
             [e #`',(syntax->datum #'e)]))])))

  (define-match (syntax-match syntax-extend-backquote)))
