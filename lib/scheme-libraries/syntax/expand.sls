#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax expand)
  (export
    expand
    expand-body
    expand-expression
    expand-library
    transform)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries helpers)
    (scheme-libraries match)
    (scheme-libraries numbers)
    (scheme-libraries parameters)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects)
    (scheme-libraries syntax variable-transformers))

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
           [(splicing-binding? t)
            (expand-splicing-expression x t)]
           [(expander-binding? t)
            ((expander-binding-proc t) x)]
           [(constant-type? t)
            (build (quote ,(constant-type-datum t)))]
           [(prim-binding? t)
            (expand-primop t x)]
           [(variable-binding? t)
            (build ,(variable-binding-symbol t))]
           [else
            (display x) (newline)
            (syntax-error #f "invalid syntax in expression context" x)])))))

  (define expand-application
    (lambda (x)
      (syntax-match x
        [(,[expand-expression -> e] ,[expand-expression -> e*] ...)
         (build (,e ,e* ...))]
        [,x (syntax-error #f "invalid application syntax" x)])))

  (define expand-splicing-expression
    (lambda (x t)
      (let ([x* ((splicing-binding-proc t) x)])
        (if (null? x*)
            (syntax-error #f "empty body" x)
            (build-begin ,(map expand-expression x*) ...)))))

  (define expand-begin-expression
    (lambda (x)
      (syntax-match x
        [(,k ,[expand-expression -> e*] ... ,[expand-expression -> e])
         (build-begin ,e* ... ,e)]
        [,x (syntax-error 'begin "invalid syntax" x)])))

  (define expand-primop
    (lambda (t x)
      (syntax-match x
        [(,k ,[expand-expression -> e*] ...)
         (let ([arity (prim-binding-arity t)]
               [name (prim-binding-name t)]
               [n (length e*)])
           (if (fxnegative? arity)
               (unless (fx>=? n (fxnot arity))
                 (syntax-error name
                   "insufficient number of arguments" e*))
               (unless (fx=? n arity)
                 (syntax-error name
                   "wrong number of arguments" e*)))
           (build (,name ,e* ...)))])))

  (define expand-body
    (lambda (x*)
      (let-values ([(def* e)
                    (expand-internal x* (make-ribcage))])
        (if (null? def*)
            e
            (let ([x* (map definition-var def*)]
                  [e* (map definition-expr def*)])
              (build (letrec* ([,x* ,e*] ...) ,e)))))))

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
         [(splicing-binding? t)
          (expand-splicing x t x* ribs rdef*)]
         [else
          (let ([e* (map expand-expression (cons x x*))])
            (values (expand-definitions (reverse rdef*))
                    (build-begin ,e* ...)))]))))

  (define expand-splicing
    (lambda (x t x* ribs rdef*)
      (let ([e* ((splicing-binding-proc t) x)])
        (expand-form* (append e* x*) ribs rdef*))))

  (define expand-definitions
    (lambda (def*)
      (map (lambda (thunk) (thunk)) def*)))

  ;; Transformations

  (define transform
    (lambda (f x ribs)
      (apply-transformer f (apply-anti-mark x) ribs)))

  (define apply-transformer
    (lambda (f x ribs)
      (let ([f (if (variable-transformer? f)
                   (variable-transformer-proc f)
                   f)])
        (guard (c
                [(invalid-syntax-object-condition? c)
                 (syntax-error #f (format "encountered invalid object ~s in output of macro"
                                    (invalid-syntax-object-irritant c)))])
          (wrap-syntax-object (f x) (make-mark) ribs)))))

  ;; syntax-type

  (define syntax-type
    (lambda (x ribs)
      (define keyword-type
        (lambda (bdg)
          (syntax-type (transform (keyword-binding-transformer bdg)
                                  x
                                  ribs)
                       ribs)))
      (syntax-match x
        [(,k . ,x*)
         (guard ($identifier? k))
         (let* ([lbl (identifier->label k)]
                [bdg (label->binding lbl)])
           (cond
            [(or (splicing-binding? bdg)
                 (expander-binding? bdg)
                 (definition-binding? bdg)
                 (prim-binding? bdg))
             (values x bdg)]
            [(keyword-binding? bdg)
             (keyword-type bdg)]
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
                   [(keyword-binding? bdg)
                    (keyword-type bdg)]
                   [(auxiliary-binding? bdg)
                    (syntax-error (auxiliary-binding-who bdg) "invalid use of auxiliary syntax" x)]
                   [(out-of-phase-binding? bdg)
                    (syntax-error #f "identifier referenced out of phase" x)]
                   [(displaced-binding? bdg)
                    (syntax-error #f "identifier referenced out of context" x)]
                   [else
                    (values x (make-other-type))])))]
          [else
           (undefined-error x "unbound identifier ~a")])]
        [,x
         (let ([e (syntax-object->datum x)])
           (unless (constant? e)
             (syntax-error #f "invalid expression syntax" x))
           (values x (make-constant-type e)))])))

  ;; Libraries

  (define expand-library
    (lambda (name ver exp* imp* body*)
      ;; FIXME
      (assert #f)))

  ;; Parsers

  #;
  (define parse-library
    (lambda (x)
      (syntax-match x
        [(library ,name
           (export ,exp-spec* ...)
           (import ,imp-spec* ...)
           ,body* ...)
         (let-values ([(name ver)
                       (parameterize ([current-form x])
                         (parse-library-name name))])
           (values name ver exp-spec* imp-spec* body*))]
        [,k (syntax-error #f "invalid library syntax" x)])))

  #;
  (define/who parse-library-name
    (define doparse
      (lambda (part* sub-ver*)
        (values (map parse-library-name-part part*)
                (map parse-sub-version sub-ver*))))
    (lambda (x)
      (syntax-match x
        [(,part* ... (,sub-ver* ...))
         (guard (for-all $identifier? part*))
         (doparse part* sub-ver*)]
        [(,part* ...)
         (guard (for-all $identifier? part*))
         (doparse part* '())]
        [,x (syntax-error #f "ill-formed library name" #f x)])))

  #;
  (define parse-library-name-part
    (lambda (x)
      (unless ($identifier? x)
        (syntax-error #f "invalid library name part" #f x))
      (identifier->symbol x)))

  #;
  (define parse-sub-version
    (lambda (x)
      (let ([e (syntax-object->datum x)])
        (unless (exact-nonnegative-integer? e)
          (syntax-error #f "invalid library sub-version" #f x))
        e)))


  )
