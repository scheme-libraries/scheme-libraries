#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expand)
  (export
    expand
    expand-body
    expand-expression)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries helpers)
    (scheme-libraries match)
    (scheme-libraries parameters)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax syntax-match)
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
      (unless (environment? env)
        (assertion-violation who "invalid environment argument" env))
      (expand-expression (annotated-datum->syntax-object expr env))))

  (define expand-expression
    (lambda (x)
      (let f ([x x])
        (let-values ([(x t) (syntax-type x #f)])
          (cond
           [(application-type? t)
            (expand-application x)]
           [(expander-binding? t)
            ((expander-binding-proc t) x)]
           [(constant-type? t)
            (build (quote ,(constant-type-datum t)))]
           [(variable-binding? t)
            (build ,(variable-binding-symbol t))]
           [else
            (syntax-error #f "invalid syntax in expression context" x)])))))

  (define expand-application
    (lambda (x)
      (syntax-match x
        [(,[expand-expression -> e] ,[expand-expression -> e*] ...)
         (build (,e ,e* ...))]
        [,x (syntax-error #f "invalid application syntax" x)])))

  (define expand-body
    (lambda (x*)
      (let-values ([(def* e)
                    (expand-internal x* (make-ribcage))])
        (let ([x* (map definition-var def*)]
              [e* (map definition-expr def*)])
          (build (letrec* `([,x* ,e*] ...) ,e))))))

  (define expand-internal
    (lambda (x* ribs)
      (let ([x* (add-substitutions* ribs x*)])
        (expand-form* x* ribs '()))))

  (define expand-form*
    (lambda (x* ribs rdef*)
      (match x*
        [()
         (syntax-error #f "no expressions in body")]
        [(,x . ,x*)
         (expand-form x x* ribs rdef*)])))

  (define expand-form
    (lambda (x x* ribs rdef*)
      (let-values ([(x t) (syntax-type x ribs)])
        (cond
         [(definition-binding? t)
          (let ([def* ((definition-binding-proc t) x ribs)])
            (expand-form* x* ribs (append (reverse def*) rdef*)))]
         [else
          (let ([e* (map expand-expression (cons x x*))])
            (values (expand-definitions (reverse rdef*))
                    (build-begin ,e*)))]))))

  (define expand-definitions
    (lambda (def*)
      (map (lambda (thunk) (thunk)) def*)))

  ;; Syntax-type

  (define syntax-type
    (lambda (x ribs)
      (syntax-match x
        [(,k . ,x*)
         (guard ($identifier? k))
         (let* ([lbl (identifier->label k)]
                [bdg (label->binding lbl)])
           (cond
            [(or (expander-binding? bdg)
                 (definition-binding? bdg))
             (values x bdg)]
            [else
             (values x (make-application-type))]))]
        [(,f . ,x*)
         (values x (make-application-type))]
        [,x
         (guard ($identifier? x))
         (cond
          [(identifier->label x)
           => (lambda (lbl)
                (let ([bdg (label->binding lbl)])
                  (cond
                   [(variable-binding? bdg)
                    (values x bdg)]
                   ;; TODO
                   [else
                    (values x (make-other-type))])))]
          [else
           (undefined-error x "unbound identifier ~a")])]
        [,x
         (let ([e (syntax-object->datum x)])
           (unless (constant? e)
             (syntax-error #f "invalid expression syntax" x))
           (values x (make-constant-type e)))])))

  )
