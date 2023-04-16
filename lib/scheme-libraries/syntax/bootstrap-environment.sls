#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax bootstrap-environment)
  (export
    bootstrap-environment)
  (import
    (rnrs)
    ;; DEBUG
    (only (chezscheme) pretty-print)

    (scheme-libraries define-who)
    (scheme-libraries helpers)
    (scheme-libraries match)
    (scheme-libraries parameters)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax expand)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects)
    (scheme-libraries syntax variables)
    (scheme-libraries thread-parameters))

  (define-syntax declare-syntax
    (lambda (stx)
      (syntax-case stx ()
          [(_ name bdg)
           #`(let ([l/p (make-label/props (make-label bdg (metalevel:syntax) 'name))])
               (environment-set! (system-environment) 'name l/p))])))

  (define-syntax declare-expander-syntax
    (syntax-rules ()
      [(declare-expander-syntax name proc)
       (declare-syntax name (make-expander-binding proc))]))

  (define-syntax declare-splicing-syntax
    (syntax-rules ()
      [(declare-splicing-syntax name proc)
       (declare-syntax name (make-splicing-binding proc))]))

  (define-syntax declare-definition-syntax
    (syntax-rules ()
      [(declare-expander-syntax name proc)
       (declare-syntax name (make-definition-binding proc))]))

  (define-syntax declare-auxiliary-syntax
    (syntax-rules ()
      [(declare-expander-syntax name)
       (declare-syntax name (make-auxiliary-binding 'name))]))

  (define-syntax declare-prim-syntax
    (syntax-rules ()
      [(declare-expander-syntax name arity)
       (declare-syntax name (make-prim-binding 'name arity))]))

  ;; Helpers

  (define parse-define
    (lambda (x)
      (define who 'define)
      (syntax-match x
        [(,k ,x ,e)
         (guard ($identifier? x))
	 (values x e)]
        [(,k ,x)
         (guard ($identifier? x))
         (values x `(void))]
	[(,k (,x . ,formals) ,e)
	 (guard ($identifier? x))
	 (values x `(lambda ,formals ,e))]
        [,x
         (syntax-error who "invalid syntax" x)])))

  (define parse-define-syntax
    (lambda (x)
      (define who 'define-syntax)
      (syntax-match x
        [(,k ,x ,e)
         (guard ($identifier? x))
         (values x e)]
        [,x (syntax-error who "invalid syntax" x)])))

  (define parse-formals
    (lambda (who form x)
      (syntax-match x
        [(,x* ...)
         (guard (valid-bound-identifiers? x*))
         x*]
        [(,x* ... . ,x)
         (guard (valid-bound-identifiers? (cons x x*)))
         `(,x* ... . ,x)]
        [,x (syntax-error who "invalid syntax")])))

  (define formals-map
    (lambda (proc formals)
      (match formals
        [() '()]
        [(,x . ,[x*]) `(,(proc x) . ,x*)]
        [,x (proc x)])))

  (define formals->list
    (lambda (formals)
      (match formals
        [() '()]
        [(,x . ,[x*]) `(,x . ,x*)]
        [,x `(,x)])))

  (define valid-bound-identifiers?
    (lambda (stx*)
      ;; TODO: The algorithm is currently quadratic, which may matter
      ;; for generated code.
      (let f ([stx* stx*]
	      [id* '()])
	(or (null? stx*)
	    (let ([stx (car stx*)])
	      (and ($identifier? stx)
		   (not (find
			 (lambda (id)
			   ($bound-identifier=? id stx))
			 id*))
		   (f (cdr stx*) (cons stx id*))))))))

  (define $ellipsis?
    (lambda (x)
      (and ($identifier? x)
           ($free-identifier=? x (syntax-extend-backquote here `...)))))

  (define expand-letrec
    (lambda (x who)
      (syntax-match x
        [(,k ([,x* ,e*] ...) ,b* ... ,b)
         (guard (for-all $identifier? x*))
         (unless (valid-bound-identifiers? x*)
           (syntax-error who "invalid syntax" x))
         (let* ([name* (map identifier->symbol x*)]
                [var* (map make-variable name*)]
                [bdg* (map make-variable-binding var*)]
                [lbl* (map make-label bdg*)]
                [ribs (make-ribcage x* lbl*)]
                [e* (add-substitutions* ribs e*)]
                [form* (add-substitutions* ribs `(,b* ... ,b))])
           (parameterize ([current-who who]
                          [current-form x])
             (let ([e* (map expand-expression e*)]
                   [e (expand-body form*)])
               (for-each label-kill! lbl*)
               (build (,who ([,var* ,e*] ...) ,e)))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (define expand-let-syntax
    (lambda (x who)
      (syntax-match x
        [(,k ([,x* ,e*] ...) ,b* ...)
         (guard (for-all $identifier? x*))
         (let* ([lbl* (map
                        (lambda (x)
                          (make-label #f (current-metalevel-for-syntax)))
                        x*)]
                [ribs (make-ribcage x* lbl*)]
                [bdg*
                 (map
                   (lambda (x lbl e)
                     (let* ([e (case who
                                 [(let-syntax) e]
                                 [(letrec-syntax)
                                  (add-substitutions ribs e)]
                                 [else #f])]
                            [proc (eval-transformer e who)])
                       (make-keyword-binding proc)))
                   x* lbl* e*)])
           (for-each label-bind! lbl* bdg*)
           (add-substitutions* ribs b*))]
        [,x (syntax-error who "invalid syntax" x)])))

  ;; Macros

  (define eval-transformer
    (lambda (x who)
      (let ([e (meta-expand x)])
        (let ([f (execute-transformer e)])
          ;; TODO: variable transformer
          (unless (procedure? f)
            (assertion-violation who "invalid transformer" f))
          f))))

  (define meta-expand
    (lambda (x)
      ;; TODO: Switch context and collect information about visited
      ;; symbols.
      (let ([e (parameterize ([current-metalevel
                               (fx+ (current-metalevel) 1)])
                 (expand-expression x))])
        e)))

  (define execute-transformer
    (lambda (e)
      ;; TODO Wrap e with code so that the runtime globals coming from
      ;; the expansion of e (in the meta-context) are set.  (The
      ;; values are also delivered in a vector.)
      (execute e)))

  (define execute
    (lambda (e)
      ;; TODO: Install a continuation barrier.
      ((compile-to-thunk e))))

  ;; Syntax-case

  (define/who syntax-case-expander
    (define who 'syntax-case)
    (define-record-type pattern-variable
      (nongenerative) (sealed #t)
      (fields pattern identifier level))
    (lambda (x)
      (let-values ([(e lit* cl*) (parse-syntax-case x)])
        (define literal?
          (lambda (id)
            (exists (lambda (lit) ($bound-identifier=? id lit)) lit*)))
        (define gen-clause
          (lambda (cl e f)
            (define gen-matcher
              (lambda (pat e)
                (syntax-match pat
                  ;; (<pattern> <ellipsis> . <pattern>)
                  [(,pat1 ,ell . ,pat2)
                   (guard ($ellipsis? ell))
                   (let ([l (syntax-length+ pat2)]
                         [e1 (generate-temporary)]
                         [e2 (generate-temporary)]
                         [n (generate-temporary)])
                     (let*-values ([(mat1 pvar1*) (gen-map pat1 e1)]
                                   [(mat2 pvar2*) (gen-matcher* pat2 e2)])
                       (values
                         (lambda (k)
                           `(syntax-split ,e ,l
                              (lambda (,e1 ,e2)
                                ,(mat1 (lambda () (mat2 k))))
                              (lambda () ,(f))))
                         (append pvar1* pvar2*))))]
                  ;; (<pattern> . <pattern>)
                  [(,pat1 . ,pat2)
                   (let ([e1 (generate-temporary)]
                         [e2 (generate-temporary)])
                     (let-values ([(mat1 pvar1*) (gen-matcher pat1 e1)]
                                  [(mat2 pvar2*) (gen-matcher pat2 e2)])
                       (values
                         (lambda (k)
                           `(if (syntax-pair? ,e)
                                ;; TODO: Introduce syntax-car+cdr.
                                (let ([,e1 (syntax-car ,e)]
                                      [,e2 (syntax-cdr ,e)])
                                  ,(mat1 (lambda () (mat2 k))))
                                ,(f)))
                         (append pvar1* pvar2*))))]
                  ;; #(<pattern> ...)
                  [#(,pat* ...)
                   (let ([t (generate-temporary)])
                     (let-values ([(mat pvar*) (gen-matcher pat* t)])
                       (values
                         (lambda (k)
                           `(if (syntax-vector? ,e)
                                (let ([,t (syntax-vector->list ,e)])
                                  ,(mat k))
                                ,(f)))
                         pvar*)))]
                  ;; <underscore>
                  [_ (values (lambda (k) (k)) '())]
                  ;; <ellipsis>
                  [... (syntax-error #f "misplaced ellipsis in syntax pattern" x pat)]
                  [,pat
                   (guard ($identifier? pat))
                   (if (literal? pat)
                       ;; <literal>
                       (values
                         (lambda (k)
                           `(if (and (identifier? ,e)
                                     (free-identifier=? ($syntax ,pat) ,e))
                                ,(k)
                                ,(f)))
                         '())
                       ;; <pattern variable>
                       (values
                         (lambda (k) (k))
                         (list (make-pattern-variable pat e 0))))]
                  ;; <constant>
                  [,pat
                   (let ([d (syntax-object->datum pat)])
                     (values
                       (lambda (k)
                         `(if (equal? ',d (syntax->datum ,e))
                              ,(k)
                              ,(f)))
                   '()))])))
            (define gen-map
              (lambda (pat e)
                (let ([g (generate-temporary)]
                      [h (generate-temporary)]
                      [l (generate-temporary)])
                  (let-values ([(mat pvar*) (gen-matcher pat g)])
                    (let ([g* (map (lambda (pvar) (generate-temporary)) pvar*)])
                      (values
                        (lambda (k)
                          (syntax-extend-backquote here
                            `(let ,l ([,h (reverse ,e)]
                                      [,g* ,(map (lambda (g) `'()) g*)]
                                      ...)
                                  (if (eq? ,h '())
                                      ,(k)
                                      (let ([,g (car ,h)])
                                        ,(mat
                                          (lambda ()
                                            `(,l (cdr ,h)
                                                 ,(map
                                                    (lambda (pvar g)
                                                      `(cons ,(pattern-variable-identifier pvar)
                                                             ,g))
                                                    pvar* g*)
                                                 ...))))))))
                        (map
                          (lambda (pvar g)
                            (make-pattern-variable
                             (pattern-variable-pattern pvar)
                             g
                             (fx+ (pattern-variable-level pvar) 1)))
                          pvar* g*)))))))
            (define gen-matcher*
              (lambda (pat e)
                (syntax-match pat
                  [(,pat1 . ,pat2)
                   (let ([e1 (generate-temporary)]
                         [e2 (generate-temporary)])
                     (let*-values ([(mat1 pvar1*) (gen-matcher pat1 e1)]
                                   [(mat2 pvar2*) (gen-matcher pat2 e2)])
                       (values
                         (lambda (k)
                           `(if (syntax-pair? ,e)
                                (let ([,e1 (syntax-car ,e)]
                                      [,e2 (syntax-cdr ,e)])
                                  ,(mat1 (lambda () (mat2 k))))
                                ,(f)))
                         (append pvar1* pvar2*))))]
                  [()
                   (values
                     (lambda (k)
                       `(if (syntax-null? ,e) ,(k) ,(f)))
                     '())]
                  [,pat
                   (gen-matcher pat e)])))
            ;; gen-clause
            (let*-values ([(pat fend out) (parse-clause cl)]
                          [(mat pvar*) (gen-matcher pat e)])
              (let ([pat* (map pattern-variable-pattern pvar*)])
                (unless (valid-bound-identifiers? pat*)
                  (syntax-error who "duplicate pattern variable" x))
                (syntax-extend-backquote here
                  (let ([pat* (map
                                (lambda (pvar)
                                  (let f ([n (pattern-variable-level pvar)])
                                    (if (fxzero? n)
                                        (pattern-variable-pattern pvar)
                                        `(,(f (fx- n 1)) (... ...)))))
                                pvar*)]
                        [id* (map pattern-variable-identifier pvar*)])
                    (mat (lambda ()
                           `($with-syntax ([,pat* ,id*] ...)
                              ,(if fend
                                   `(if ,fend ,out ,(f))
                                   out))))))))))
        (define parse-clause
          (lambda (cl)
            (syntax-match cl
              [(,pat ,fend ,out)
               (values pat fend out)]
              [(,pat ,out)
               (values pat #f out)]
              [,cl (syntax-error who "invalid clause" x cl)])))
        ;; syntax-case-expander
        (expand-expression
         (let ([t (generate-temporary)]
               [f (generate-temporary)])
           (dlog
            (syntax-extend-backquote here
              `(let ([,t ,e])
                 ,(fold-right
                    (lambda (cl rest)
                      `(let ([,f (lambda () ,rest)])
                         ,(gen-clause cl t (lambda () `(,f)))))
                    `(syntax-violation #f "invalid syntax" ,t)
                    cl*)))))))))

  ;; FIXME: DEBUG: XXX: LOG
  ;; Move this into execute-transformer code.
  (define dlog (lambda (x)
                 #;
                 (pretty-print (syntax-object->datum x))
                x))

  (define parse-syntax-case
    (lambda (x)
      (syntax-match x
        [(,k ,e (,lit* ...) ,cl* ...)
         (guard (for-all
                 (lambda (x)
                   (and ($identifier? x)
                        (not ($free-identifier=? x `...))
                        (not ($free-identifier=? x `_))))
                 lit*))
         (values e lit* cl*)])))

  (define/who syntax-expander
    ;; XXX: Check the meaning of tail.
    (define lookup-pattern-variable
      (lambda (x)
        (let ([bdg (label->binding (identifier->label x))])
          (and (pattern-variable-binding? bdg)
               bdg))))
    (define extend-envs
      (lambda (n env*)
        (let f ([n n] [env* env*])
          (let ([env* (cons '() env*)])
            (if (fx=? n 1)
                env*
                (f (fx- n 1) env*))))))
    (lambda (depth)
      (lambda (x)
        (define gen-template
          (lambda (tmpl depth)
            (let f ([tmpl tmpl]
                    [depth depth]
                    [env* '()]
                    [ell? (lambda (x) ($ellipsis? x))]
                    [tail? #f])
              (define update-envs
                (lambda (in out lvl env*)
                  (let f ([in in]
                          [lvl lvl]
                          [env* env*])
                    (cond
                     [(null? env*)
                      (syntax-error #f "too few ellipses following syntax template" x tmpl)]
                     [(fx=? lvl 1)
                      (cons (cons (cons in out)
                                  (car env*))
                            (cdr env*))]
                     [else
                      (let ([out (generate-temporary)])
                        (cons (cons (cons in out)
                                    (car env*))
                              (f out
                                 (fx- lvl 1)
                                 (cdr env*))))]))))
              (syntax-match tmpl
                ;; (<ellipsis> <template>)
                [(,ell ,tmpl1)
                 (guard (ell? ell))
                 (let-values ([(out env* var?)
                               (f tmpl1 depth env* (lambda (x) #f) tail?)])
                   (if var?
                       (values out env* #t)
                       (values `($syntax ,tmpl1) env* #t)))]
                ;; (quasisyntax <template>)
                [(,qs ,tmpl1)
                 (guard depth ($identifier? qs) ($free-identifier=? qs `quasisyntax))
                 (let-values ([(out env* var?)
                               (f tmpl1 (fx+ depth 1) env* ell? #f)])
                   (if var?
                       (values `(list ($syntax ,qs) ,out) env* #f)
                       (values `($syntax ,tmpl) env* #f)))]
                ;; (unsyntax <expression>)
                [(unsyntax ,out)
                 (guard depth (fxzero? depth))
                 (values out env* #t)]
                ;; (unsyntax <template>)
                [(,us ,tmpl1)
                 (guard depth ($identifier? us) ($free-identifier=? us `unsyntax))
                 (let-values ([(out env* var?)
                               (f tmpl1 (fx- depth 1) env* ell? #f)])
                   (if var?
                       (values `(list ($syntax ,us) ,out) env* #f)
                       (values `($syntax ,tmpl) env* #f)))]
                ;; ((unsyntax-splicing <template> ...) . <template>)

                ;; TODO

                ;; ((unsyntax <template> ...) . <template>)

                ;; TODO

                ;; (<template> <ellipsis> ... . <template>)
                [(,tmpl1 ,ell . ,tmpl2)
                 (guard (ell? ell))
                 (let g ([tmpl2 tmpl2] [lvl 1])
                   (define pop-env
                     ;; TODO: Use descriptive record types for env.
                     (lambda (env*)
                       (let ([env (car env*)])
                         (when (null? env)
                           (syntax-error who "too many ellipses following syntax template" x tmpl1))
                         (values
                           (map car env)
                           (map
                             (lambda (bdg)
                               (let ([out (cdr bdg)])
                                 out))
                             env)
                           (cdr env*)))))
                   (syntax-match tmpl2
                     [(,ell . ,tmpl2)
                      (guard (ell? ell))
                      (g tmpl2 (fx+ lvl 1))]
                     [,tmpl2
                      (let*-values ([(out2 env* var?) (f tmpl2 depth env*  ell? #t)]
                                    [(out1 env* var?) (f tmpl1 depth (extend-envs lvl env*)
                                                         ell? #f)]
                                    [(out env*)
                                     (let f ([lvl lvl])
                                       (if (fx=? lvl 1)
                                           (let-values ([(var* init* env*)
                                                         (pop-env env*)])
                                             (values
                                               `(map
                                                  (lambda (,var* ...)
                                                    ,out1)
                                                  ,init* ...)
                                               env*))
                                           (let*-values ([(out env*)
                                                          (f (fx- lvl 1))]
                                                         [(var* init* env*)
                                                          (pop-env env*)])
                                             (values
                                               ;; FIXME: Provide append-map in runtime.
                                               `(append-map
                                                 (lambda (,var* ...)
                                                   ,out)
                                                 ,init* ...)
                                               env*))))])
                        (values `(append ,out ,out2)
                                env*
                                #t))]))]
                ;; (<template> . <template>)
                [(,tmpl1 . ,tmpl2)
                 (let*-values ([(out1 env* var1?)
                                (f tmpl1 depth env* ell? #f)]
                               [(out2 env* var2?)
                                (f tmpl2 depth env* ell? #f)])
                   (if (or var1? var2?)
                       (values `(cons ,out1 ,out2) env* #t)
                       (values `($syntax ,tmpl) #f)))]
                ;; #(<subtemplate> ...)
                [#(,tmpl* ...)
                 (let-values ([(out env* var?) (f tmpl* depth env* ell? #f)])
                   (values (if var?
                               `(syntax-list->vector ,out)
                               `($syntax ,tmpl))
                           env*
                           var?))]
                ;; <identifier>
                [,tmpl
                 (guard ($identifier? tmpl))
                 (cond
                  [(lookup-pattern-variable tmpl)
                   => (lambda (bdg)
                        (let ([id (pattern-variable-binding-identifier bdg)]
                              [lvl (pattern-variable-binding-level bdg)])
                          (if (fxzero? lvl)
                              (values id env* #t)
                              (let ([tmp (generate-temporary)])
                                (values tmp
                                        (update-envs tmp id lvl env*)
                                        #t)))))]
                  [else
                   (values `($syntax ,tmpl) env* #f)])]
                ;; <constant>
                [,tmpl
                 (values
                   (if (and tail? (syntax-null? tmpl))
                       `(quote ())
                       `($syntax ,tmpl))
                   env*
                   #f)]))))
        (syntax-match x
          [(,k ,tmpl)
           (let-values ([(out env* var?)
                         (gen-template tmpl depth)])
             (expand-expression (dlog out)))]
          [,x
           (syntax-error #f "invalid syntax" x)]))))

  ;; Bootstrap environment

  (define bootstrap-environment
    (lambda ()
      ;; TODO: Copy the system environment.
      (system-environment)))

  ;; Syntax

  ;; Auxiliary syntax

  (declare-auxiliary-syntax =>)
  (declare-auxiliary-syntax else)
  (declare-auxiliary-syntax ...)
  (declare-auxiliary-syntax _)
  (declare-auxiliary-syntax unquote)
  (declare-auxiliary-syntax unquote-splicing)
  (declare-auxiliary-syntax unsyntax)
  (declare-auxiliary-syntax unsyntax-splicing)

  ;; Definitions

  (declare-definition-syntax define
    (lambda (x ribs)
      (let-values ([(x e) (parse-define x)])
        (let* ([var (make-variable (identifier->symbol x))]
               [bdg (make-variable-binding var)]
               [lbl (ribcage-add! ribs x bdg)])
          (unless lbl
            (identifier-error 'define x "trying to redefine the local keyword ~a"))
          (list (lambda ()
                  (make-definition var (expand-expression e))))))))

  (declare-definition-syntax define-syntax
    (lambda (x ribs)
      (define who 'define-syntax)
      (let-values ([(x e) (parse-define-syntax x)])
        (let* ([proc (eval-transformer e who)]
               [bdg (make-keyword-binding proc)]
               [lbl (ribcage-add! ribs x bdg (current-metalevel-for-syntax))])
          (unless lbl
            (identifier-error 'define-syntax x "trying to redefine the local keyword ~a"))
          '()))))

  ;; Splicing syntax

  (declare-splicing-syntax begin
    (lambda (x)
      (define who 'begin)
      (syntax-match x
        [(,k ,x* ...)
         x*]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-splicing-syntax let-syntax
    (lambda (x)
      (expand-let-syntax x 'let-syntax)))

  (declare-splicing-syntax letrec-syntax
    (lambda (x)
      (expand-let-syntax x 'letrec-syntax)))

  ;; Expanders

  (declare-expander-syntax lambda
    (lambda (x)
      (define who 'lambda)
      (syntax-match x
        [(,k ,formals ,body* ... ,body)
         (let* ([formals (parse-formals who x formals)]
                [names (formals-map identifier->symbol formals)]
                [vars (formals-map make-variable names)]
                [id* (formals->list formals)]
                [bdg* (map make-variable-binding (formals->list vars))]
                [lbl* (map make-label bdg*)]
                [ribs (make-ribcage id* lbl*)]
                [form* (add-substitutions* ribs `(,body* ... ,body))]
                [e (parameterize ([current-who who]
                                  [current-form x])
                     (expand-body form*))])
           (for-each label-kill! lbl*)
           (build
             (lambda ,vars ,e)))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax if
    (lambda (x)
      (define who 'if)
      (syntax-match x
        [(,k ,[expand-expression -> e0] ,[expand-expression -> e1] ,[expand-expression -> e2])
         (build (if ,e0 ,e1 ,e2))]
        [(,k ,[expand-expression -> e0] ,[expand-expression -> e1])
         (build (if ,e0 ,e1 (values)))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax quote
    (lambda (x)
      (define who 'quote)
      (syntax-match x
        [(,k ,d)
         (build (quote ,(syntax-object->datum d)))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax set!
    (lambda (x)
      (define who 'set!)
      (syntax-match x
        [(,k ,x ,[expand-expression -> e])
         (guard ($identifier? x))
         ;; TODO: Variable transformers.
         `(set! ,x ,e)]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax and
    (lambda (x)
      (define who 'and)
      (syntax-match x
        [(,k) (build (quote #t))]
        [(,k ,[expand-expression -> e] ,[expand-expression -> e*] ...)
         (let f ([e e] [e* e*])
           (if (null? e*)
               e
               (build (if ,e ,(f (car e*) (cdr e*)) '#f))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax or
    (lambda (x)
      (define who 'or)
      (syntax-match x
        [(,k) (build (quote #f))]
        [(,k ,[expand-expression -> e] ,[expand-expression -> e*] ...)
         (let f ([e e] [e* e*])
           (if (null? e*)
               e
               (let ([t (make-variable 't)])
                 (build-let ([,t ,e]) (if ,t ,t ,(f (car e*) (cdr e*)))))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax let
    (lambda (x)
      (define who 'let)
      (syntax-match x
        [(,k ([,x* ,e*] ...) ,b* ... ,b)
         (guard (for-all $identifier? x*))
         (expand-expression `((lambda ,x* ,b* ... ,b) ,e* ...))]
        [(,k ,f ([,x* ,e*] ...) ,b* ... ,b)
         (guard ($identifier? f) (for-all $identifier? x*))
         (expand-expression `(letrec ([,f (lambda ,x* ,b* ... ,b)]) (,f ,e* ...)))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax letrec
    (lambda (x)
      (expand-letrec x 'letrec)))

  (declare-expander-syntax letrec*
    (lambda (x)
      (expand-letrec x 'letrec*)))

  (declare-expander-syntax let*
    (lambda (x)
      (define who 'let*)
      (syntax-match x
        [(,k () ,b* ... ,b)
         (expand-body `(,b* ... ,b))]
        [(,k ([,x ,e] [,x* ,e*] ...) ,b* ... ,b)
         (guard (for-all $identifier? x*))
         (let f ([x x] [x* x*] [e e] [e* e*])
           (if (null? x*)
               (expand `(let ([,x ,e]) ,b* ... ,b))
               (expand `(let ([,x ,e])
                          ,(f (car x*) (cdr x*)
                              (car e*) (cdr e*))))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax cond
    (lambda (x)
      (define who 'cond)
      (syntax-match x
        [(,k ,cl ,cl* ...)
         (let f ([cl cl] [cl* cl*])
           (if (null? cl*)
               (syntax-match cl
                 [(else ,[expand-expression -> e] ,[expand-expression -> e*] ...)
                  (build-begin ,e ,e* ...)]
                 [(,[expand-expression -> t] => ,[expand-expression -> e])
                  (let ([x (make-variable 't)])
                    (build-let ([,x ,t])
                      (if ,x (,e ,x) (values))))]
                 [(,[expand-expression -> t])
                  (let ([x (make-variable 't)])
                    (build-let ([,x ,t])
                      (if ,x ,x (values))))]
                 [(,[expand-expression -> t] ,[expand-expression -> e*] ...)
                  (build (if ,t ,(build-begin ,e* ...) (values)))]
                 [,cl (syntax-error who "invalid clause" x cl)])
               (let ([rest (f (car cl*) (cdr cl*))])
                 (syntax-match cl
                   [(,[expand-expression -> t] => ,[expand-expression -> e])
                    (let ([x (make-variable 't)])
                      (build-let ([,x ,t])
                        (if ,x (,e ,x) ,rest)))]
                   [(,[expand-expression -> t])
                    (let ([x (make-variable 't)])
                      (build-let ([,x ,t])
                        (if ,x ,x ,rest)))]
                   [(,[expand-expression -> t] ,[expand-expression -> e*] ...)
                    (build (if ,t ,(build-begin ,e* ...) ,rest))]
                   [,cl (syntax-error who "invalid clause" x cl)]))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax case
    (lambda (x)
      (define who 'case)
      (syntax-match x
        [(,k ,e ,cl ,cl* ...)
         (let ([t (generate-temporary)])
           (expand-expression
            `(let ([,t ,e])
               (cond
                ,(let f ([cl cl] [cl* cl*])
                   (if (null? cl*)
                       (syntax-match cl
                         [[else ,e ,e* ...]
                          `[else ,e ,e* ...]]
                         [[(,d ...) ,e ,e* ...]
                          `[(memv ,t '(,d ...)) ,e ,e* ...]]
                         [,cl (syntax-error who "invalid clause" x cl)])
                       (let ([rest (f (car cl*) (cdr cl*))])
                         (syntax-match cl
                           [[(,d ...) ,e ,e* ...]
                            `[(memv ,t '(,d ...)) ,e ,e* ...]]
                           [,cl (syntax-error who "invalid clause" x cl)]))))))))]
        [,x (syntax-error who "invalid sytnax" x)])))

  (declare-expander-syntax syntax-case
    syntax-case-expander)

  (declare-expander-syntax syntax
    (syntax-expander #f))

  (declare-expander-syntax quasisyntax
    (syntax-expander 0))

  (declare-expander-syntax with-syntax
    (lambda (x)
      (define who 'with-syntax)
      (syntax-match x
        [(,k ([,p* ,e*] ...) ,b* ... ,b)
         (expand-expression
          `(syntax-case (list ,e* ...) ()
             [(,p* ...) (let* () ,b* ... ,b)]))]
        [,x (syntax-error who "invalid syntax" x)])))

  ;; Internal syntax

  (declare-expander-syntax $syntax
    (lambda (x)
      (define who '$syntax)
      (syntax-match x
        [(,k ,e) (build (quote ,e))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax $with-syntax
    (lambda (x)
      (define who '$with-syntax)
      (define-record-type pattern-variable
        (nongenerative) (sealed #t)
        (fields identifier level))
      (define parse-pattern
        (lambda (pat)
          (let f ([pat pat] [lvl 0])
            (syntax-match pat
              [,pat
               (guard ($identifier? pat))
               (make-pattern-variable pat lvl)]
              [(,pat ,ell)
               ($ellipsis? ell)
               (f pat (fx+ lvl 1))]
              [,pat (syntax-error who "invalid pattern" pat)]))))
      (syntax-match x
        [(,k ([,pat* ,id*] ...) ,b)
         (guard (for-all $identifier? id*))
         (let* ([pvar* (map parse-pattern pat*)]
                [x* (map pattern-variable-identifier pvar*)])
           (unless (valid-bound-identifiers? x*)
             (syntax-error who "invalid syntax" x))
           (let* ([bdg* (map (lambda (pvar id)
                               (make-pattern-variable-binding
                                id (pattern-variable-level pvar)))
                             pvar* id*)]
                  [lbl* (map make-label bdg*)]
                  [ribs (make-ribcage x* lbl*)]
                  [b (add-substitutions ribs b)]
                  [e (expand-expression b)])
             (for-each label-kill! lbl*)
             e))]
        [,x (syntax-error who "invalid syntax" x)])))

  ;; prims

  (declare-prim-syntax append (fxnot 0))
  (declare-prim-syntax car 1)
  (declare-prim-syntax cdr 1)
  (declare-prim-syntax cons 2)
  (declare-prim-syntax eq? 2)
  (declare-prim-syntax equal? 2)
  (declare-prim-syntax void 0)
  (declare-prim-syntax list (fxnot 0))
  (declare-prim-syntax map (fxnot 1))
  (declare-prim-syntax memv 2)
  (declare-prim-syntax identifier? 1)
  (declare-prim-syntax free-identifier=? 2)
  (declare-prim-syntax reverse 1)
  (declare-prim-syntax syntax-car 1)
  (declare-prim-syntax syntax-cdr 1)
  (declare-prim-syntax syntax-null? 1)
  (declare-prim-syntax syntax-pair? 1)
  (declare-prim-syntax syntax->datum 1)
  (declare-prim-syntax syntax-list->vector 1)
  (declare-prim-syntax syntax-split 4)
  (declare-prim-syntax syntax-vector? 1)
  (declare-prim-syntax syntax-vector->list 1)
  (declare-prim-syntax syntax-violation (fxnot 3))

  ;; DEBUG
  (declare-prim-syntax display 1)
  (declare-prim-syntax newline 0)

  )
