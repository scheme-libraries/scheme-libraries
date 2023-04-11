#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expand)
  (export
    expand)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries parameters)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax syntax-objects))

  ;; Library collections

  (define-record-type library-collection
    (nongenerative library-collection-fa5e52df-c5a9-4d3b-ac3c-2f1c8cd9ad5a)
    (sealed #t)
    (fields ))

  (define/who current-library-collection
    (make-parameter (make-library-collection)
      (lambda (x)
        (unless (library-collection? x)
          (assertion-violation who "invalid library collection" x))
        x)))

  ;; Expander

  (define/who expand
    (lambda (expr env)
      (unless (annotated-datum? expr)
        (assertion-violation who "invalid expression argument" expr))
      (unless ($environment? env)
        (assertion-violation who "invalid environment argument" env))
      (expand-expression (annotated-datum->syntax-object expr env) metalevel:run)))

  (define expand-expression
    (lambda (x ml)
      (let f ([x x])
        (let-values ([(x type) (syntax-type x ml #f)])
          (cond
           [else
            (syntax-error #f "invalid syntax in expression context" x)])))))

  ;; Syntax-type

  (define syntax-type
    (lambda (x ml ribs)
      (syntax-match x
        [,x
         (let ([e (syntax-object->datum x)])
           (unless (constant? e)
             (syntax-error #f "invalid expression syntax" x))
           (values x (make-constant-binding e)))])))

  )
