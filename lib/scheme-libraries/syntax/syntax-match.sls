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
    (scheme-libraries atoms)
    (scheme-libraries define-match)
    (scheme-libraries helpers)
    (scheme-libraries syntax syntax-objects)
    (rename (scheme-libraries syntax syntax-objects)
            (syntax-car match-car)
            (syntax-cdr match-cdr)
            (syntax-pair? match-pair?)
            (syntax-null? match-null?)
            (syntax-vector? match-vector?)
            (syntax-vector->list match-syntax-vector->list)))

  (define match-equal?
    (lambda (x y)
      (and (syntax-atom? x)
           (syntax-atom? y)
           (if ($identifier? x)
               (and ($identifier? y)
                    ($free-identifier=? x y))
               (atom=? (syntax-object->datum x)
                       (syntax-object->datum y))))))

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
              (construct-name #'x "$" #'x)]
             [e #`'#,(syntax-object->datum #'e)]))])))

  (define-match (syntax-match syntax-extend-backquote)))
