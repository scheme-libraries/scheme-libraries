#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax bootstrap-environment)
  (export
    bootstrap-environment
    bootstrap-library-collection
    library-collection
    label->datum
    datum->label
    label/props->datum
    datum->label/props
    )
  (import
    (rnrs)
    ;; DEBUG
    (only (chezscheme) pretty-print trace-define)

    (scheme-libraries define-who)
    (scheme-libraries helpers)
    (scheme-libraries match)
    (scheme-libraries parameters)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries gensyms)
    (scheme-libraries hashtables)
    (scheme-libraries uuid)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax expand)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax library-collections)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects)
    (scheme-libraries syntax variables)
    (scheme-libraries syntax variable-transformers)
    (scheme-libraries syntax $environments)
    (scheme-libraries syntax $marks)
    (scheme-libraries syntax $metalevels)
    (scheme-libraries syntax $labels)
    (scheme-libraries syntax $ribs)
    (scheme-libraries thread-parameters))

  (define-syntax declare-syntax
    (lambda (stx)
      (syntax-case stx ()
          [(_ name bdg)
           #`(let* ([lbl (make-label bdg (metalevel:syntax) 'name)]
                    [l/p (make-label/props lbl)])
               (hashtable-set! symbol-table 'name lbl)
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

  ;; Conversions

  (define symbol-table
    (make-eq-hashtable))

  (define label->datum
    (lambda (lbl)
      (assert (label? lbl))
      (label-name lbl)))

  (define datum->label
    (lambda (s)
      (assert (symbol? s))
      (or (hashtable-ref symbol-table s #f)
          (symbol->object s (lambda ()
                              (make-label #f (metalevel:syntax) s))))))

  (define label/props->datum
    (lambda (l/p)
      (assert (label/props? l/p))
      (cons (label->datum (label/props-label l/p))
            (map label->datum (label/props-props l/p)))))

  (define datum->label/props
    (lambda (ls)
      (assert (and (pair? ls)
                   (list? (cdr ls))))
      (make-label/props (datum->label (car ls))
                        (map datum->label (cdr ls)))))

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
                [ribs (ribcage x* lbl*)]
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
                [ribs (ribcage x* lbl*)]
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
      (let-values ([(vars locs e) (meta-expand x)])
        (let ([f (execute-transformer vars locs e)])
          (unless (or (procedure? f)
                      (variable-transformer? f))
            (assertion-violation who "invalid transformer" f))
          f))))

  (define meta-expand
    (lambda (x)
      (with-requirements-collector
        (parameterize ([current-metalevel (fx+ (current-metalevel) 1)])
          (let ([e (expand-expression x)])
            (let-values ([(var* lib* lbl*) (current-runtime-globals)])
              (define loc* (vector-map variable-binding-location
                                (vector-map label-binding lbl*)))
              (vector-for-each
               (lambda (var lib lbl)
                 (require-for-expand! lib var lbl)
                 (library-invoke! lib))
               var* lib* lbl*)
              (values var* loc* e)))))))

  (define execute-transformer
    (lambda (vars locs e)
      (execute
       (build
         (letrec ,(map (lambda (var loc)
                         `[,var (unbox ',loc)])
                       (vector->list vars)
                       (vector->list locs))
           ;; XXX: egg2 uses set!
           ,e)))))

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
                [((unsyntax-splicing ,expr* ...) . ,tmpl1)
                 (guard depth (fxzero? depth))
                 (let-values ([(out env* var?)
                               (f tmpl1 0 env* ell? #t)])
                   (values `(append (syntax-list ,expr*) ... ,out)
                           env*
                           #t))]
                [((,us ,tmpl* ...) . ,tmpl1)
                 (guard depth ($identifier? us) ($free-identifier=? us `unsyntax-splicing))
                 (let*-values ([(out* env* var*?)
                                (f tmpl* (fx- depth 1) env* ell? #f)]
                               [(out1 env* var1?)
                                (f tmpl1 depth env* ell? #f)])
                   (if (or var*? var1?)
                       (values `(cons (list ($syntax ,us) ,out*) ,out1)
                               env*
                               #f)
                       (values `($syntax ,tmpl) env* #f)))]
                ;; ((unsyntax <template> ...) . <template>)
                [((unsyntax ,expr* ...) . ,tmpl1)
                 (guard depth (fxzero? depth))
                 (let-values ([(out env* var?)
                               (f tmpl1 0 env* ell? #t)])
                   (values `(cons* ,expr* ... ,out) env* #t))]
                [((,us ,tmpl* ...) . ,tmpl1)
                 (guard depth ($identifier? us) ($free-identifier=? us `unsyntax))
                 (let*-values ([(out* env* var*?)
                                (f tmpl* (fx- depth 1) env* ell? #f)]
                               [(out1 env* var1?)
                                (f tmpl1 depth env* ell? #f)])
                   (if (or var*? var1?)
                       (values `(cons (list ($syntax ,us) ,out*) ,out1)
                               env*
                               #f)
                       (values `($syntax ,tmpl) env* #f)))]
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
                       (values `($syntax ,tmpl) env* #f)))]
                ;; #(<subtemplate> ...)
                [#(,tmpl* ...)
                 (let-values ([(out env* var?) (f tmpl* depth env* ell? #f)])
                   (values (if var?
                               `(syntax-list->vector ,out)
                               `($syntax ,tmpl))
                           env*
                           var?))]
                ;; quasisyntax
                [quasisyntax
                 (guard depth)
                 (syntax-error who "misplaced quasisyntax in template" tmpl)]
                ;; unsyntax
                [unsyntax
                 (guard depth)
                 (syntax-error who "misplaced unsyntax in template" tmpl)]
                ;; unsyntax-splicing
                [unsyntax-splicing
                 (guard depth)
                 (syntax-error who "misplaced unsyntax-splicing in template" tmpl)]
                ;; unsyntax
                [,ell
                 (guard ($ellipsis? ell))
                 (syntax-error who "misplaced ellipsis in template" tmpl)]
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

  ;; Bootstrap library collection

  (define bootstrap-library-collection
    (lambda ()
      (let ([lc (make-library-collection)])
        (parameterize ([current-library-collection lc])
          (library-set! '($system) (make-system-library))
          lc))))

  (define make-system-library
    (lambda ()
      (make-library
       ;; Name
       '($system)
       ;; Version
       '()
       ;; Uid
       (uid '$system)
       ;; Imports
       '#()
       ;; Exports
       (environment-rib (system-environment))
       ;; Visit requirements
       '#()
       ;; Invoke requirements
       '#()
       ;; Visit code
       '()
       ;; Invoke code
       '()
       ;; Visiter
       #f
       ;; Invoker
       #f
       ;; Bindings
       '())))

  (define library-collection
    (lambda lib*
      (let ([lc (make-library-collection)])
        (parameterize ([current-library-collection lc])
          (for-each
            (lambda (lib)
              (library-set! (library-name lib) lib))
            lib*)
          lc))))

  ;; Library collection serializing

  (define library-collection->datum
    (lambda (lc)
      (parameterize ([current-library-collection lc])
        (define lib* (library-list))
        (define library->index
          (let ([ht (make-eq-hashtable)])
            (do ([i 0 (fx+ i 1)]
                 [lib* lib* (cdr lib*)])
                ((null? lib*)
                 (lambda (idx)
                   (assert (hashtable-ref ht idx #f))))
              (hashtable-set! ht (car lib*) i))))
        (map (lambda (lib)
               (library->datum library->index lib))
             lib*))))

  (trace-define library->datum
    (lambda (library->index lib)
      (define definition->datum
        (lambda (def)
          `(,(definition-var def) ,(definition-expr def))))
      (define binding->datum
        (lambda (lbl)
          (define bdg (label-binding lbl))
          (cond
           [(variable-binding? bdg)
            `(,(label->datum lbl) variable ,(variable-binding-symbol bdg))]
           [(global-keyword-binding? bdg)
            ;; XXX: Do we have to save more?
            `(,(label->datum lbl) keyword)]
           [else
            (assert #f)])))
      (list (library-name lib)
            (library-version lib)
            (library-uid lib)
            (exports->datum (library-exports lib))
            (vector-map library->index (library-visit-requirements lib))
            (vector-map library->index (library-invoke-requirements lib))
            (library-visit-commands lib) ;FIXME: unlink
            (map definition->datum (library-invoke-definitions lib)) ;FIXME: unlink
            (map binding->datum (library-bindings lib))      ;FIXME
            )))

  (define exports->datum
    (lambda (exports)
      (rib-map
       (lambda (n m l/p)
         (assert (null? m))
         (cons n (label/props->datum l/p)))
       exports)))

  (define datum->library-collection
    (lambda (rep)
      (define library-table (make-eqv-hashtable))
      (define index->library
        (lambda (idx)
          (assert (hashtable-ref library-table idx #f))))
      (assert (list? rep))
      (parameterize ([current-library-collection (make-library-collection)])
        (do ([i 0 (fx+ i 1)]
             [lib* rep (cdr lib*)])
            ((null? lib*))
          (let ([lib (datum->library index->library (car lib*))])
            (hashtable-set! library-table i lib)
            (library-set! (library-name lib) lib)))
        ;; XXX: bind any globals after loading?
        (current-library-collection))))

  (define datum->library
    (lambda (index->library obj)
      (define datum->definition
        (lambda (e)
          (match e
            [(,var ,expr) (make-definition var expr)])))
      ;; XXX: The library needs to know its runtime globals (for the later invoker).
      ;; This should just be a list of labels!
      (match obj
        [(,name ,ver ,uid ,exp* ,visreqs ,invreqs ,viscode ,invcode ((,lbl* . ,type*) ...))
         (let* ([lbl* (map datum->label lbl*)]
                [lib
                (make-library
                 ;; Name
                 name
                 ;; Version
                 ver
                 ;; Uid
                 uid
                 ;; Imports
                 '#()                    ;FIXME
                 ;; Export
                 (datum->exports exp*)
                 ;; Visit requirements
                 (vector-map index->library visreqs)
                 ;; Invoke requirements
                 (vector-map index->library invreqs)
                 ;; Visit commands
                 viscode                       ;FIXME: link
                 ;; Invoke definitions
                 (map datum->definition invcode) ;FIXME: link
                 ;; Visiter
                 #f
                 ;; Invoker
                 #f
                 ;; Bindings
                 lbl*)])
           (for-each
             (lambda (lbl type)
               (match type
                 [(variable ,sym)
                  (let ([bdg (make-variable-binding sym)])
                    (variable-binding-library-set! bdg lib)
                    (label-binding-set! lbl bdg))]
                 [(keyword)
                  ;; TODO: Get rid of global-keyword-binding
                  (let ([bdg (make-global-keyword-binding #f #f)])
                    (global-keyword-binding-library-set! bdg lib)
                    (label-binding-set! lbl bdg))]))
             lbl* type*)

           ;; FIXME: set visiter & invoker! (use lib) (using procedure on lib!
           lib)])))

  (define datum->exports
    (lambda (obj)
      (assert (list? obj))
      (let ([rib (make-rib)])
        (for-each
          (lambda (obj)
            (match obj
              [(,n . ,l/p)
               (rib-set! rib n '() (datum->label/props l/p))]))
          obj)
        rib)))

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
  (declare-auxiliary-syntax library)
  (declare-auxiliary-syntax export)
  (declare-auxiliary-syntax import)
  (declare-auxiliary-syntax for)
  (declare-auxiliary-syntax meta)
  (declare-auxiliary-syntax run)
  (declare-auxiliary-syntax expand)
  (declare-auxiliary-syntax library)
  (declare-auxiliary-syntax only)
  (declare-auxiliary-syntax except)
  (declare-auxiliary-syntax prefix)
  (declare-auxiliary-syntax rename)

  ;; Definitions

  ;; TODO: Return label/binding
  (declare-definition-syntax define
    (lambda (x ribs)
      (let-values ([(x e) (parse-define x)])
        (let* ([var (make-variable (identifier->symbol x))]
               [bdg (make-variable-binding var)]
               [lbl (ribcage-add! ribs x bdg)])
          (unless lbl
            (identifier-error 'define x "trying to redefine the local keyword ~a"))
          (values
            (list (lambda ()
                    (make-definition var (expand-expression e))))
            (list lbl))))))

  ;; TODO: Return label/binding
  (declare-definition-syntax define-syntax
    (lambda (x ribs)
      (define who 'define-syntax)
      (let-values ([(x e) (parse-define-syntax x)])
        (let* ([proc (eval-transformer e who)]
               [bdg (make-keyword-binding proc)]
               [lbl (ribcage-add! ribs x bdg (current-metalevel-for-syntax))])
          (unless lbl
            (identifier-error 'define-syntax x "trying to redefine the local keyword ~a"))
          (values '() (list lbl))))))

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
                [ribs (ribcage id* lbl*)]
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
    (lambda (stx)
      (define who 'set!)
      (syntax-match stx
        [(,k ,x ,e)
         (guard ($identifier? x))
         (let* ([lbl (identifier->label x)]
                [bdg (label->binding lbl)])
           (cond
            [(variable-binding? bdg)
             `(set! ,(variable-binding-symbol bdg) ,(expand-expression e))]
            [(keyword-binding? bdg)
             (let ([t (keyword-binding-transformer bdg)])
               (unless (variable-transformer? t)
                 (syntax-error who "invalid syntax" stx))
               (expand-expression (transform (variable-transformer-proc t) stx #f)))]
            [(not lbl)
             (undefined-error x "unbound identifier ~a")]
            [else
             (syntax-error who "invalid syntax" stx)]))]
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

  (declare-expander-syntax quasiquote
    (lambda (x)
      (define who 'quasiquote)
      (define unquote?
        (lambda (x)
          (and ($identifier? x)
               ($free-identifier=? x (syntax-extend-backquote here `unquote)))))
      (define gen-template
        (lambda (tmpl depth)
          (syntax-match tmpl
            [(quasiquote ,tmpl1)
             (let-values ([(out var?) (gen-template tmpl1 (fx+ depth 1))])
               (if var?
                   (values `(list 'quasiquote ,out) #t)
                   (values `',tmpl #f)))]
            [(,uq ,expr)
             (guard (unquote? uq) (fxzero? depth))
             (values expr #t)]
            [(,uq ,tmpl1)
             (guard (unquote? uq))
             (let-values ([(out var?) (gen-template tmpl1 (fx- depth 1))])
               (if var?
                   (values `(list 'unquote ,out) #t)
                   (values `',tmpl #f)))]
            [((,uq ,expr* ...) . ,tmpl1)
             (guard (unquote? uq) (fxzero? depth))
             (let-values ([(out var?) (gen-template tmpl1 depth)])
               (values `(cons* ,expr* ... ,out) #t))]
            [((,uq ,tmpl* ...) . ,tmpl1)
             (guard (unquote? uq))
             (let-values ([(out* var*?) (gen-template tmpl* (fx- depth 1))]
                          [(out1 var1?) (gen-template tmpl1 depth)])
               (if (or var*? var1?)
                   (values `(cons (cons 'unquote ,out*) ,out1) #t)
                   (values `',tmpl #f)))]
            [((unquote-splicing ,expr* ...) . ,tmpl1)
             (guard (fxzero? depth))
             (let-values ([(out var?) (gen-template tmpl1 depth)])
               (values `(append ,expr* ... ,out) #f))]
            [((unquote-splicing ,tmpl* ...) . ,tmpl1)
             (let-values ([(out* var*?) (gen-template tmpl* (fx- depth 1))]
                          [(out1 var1?) (gen-template tmpl1 depth)])
               (if (or var*? var1?)
                   (values `(cons (cons 'unquote-splicing ,out*) ,out1) #t)
                   (values `',tmpl #f)))]
            [(,tmpl1 . ,tmpl2)
             (let-values ([(out1 var1?) (gen-template tmpl1 depth)]
                          [(out2 var2?) (gen-template tmpl2 depth)])
               (if (or var1? var2?)
                   (values `(cons ,out1 ,out2) #t)
                   (values `',tmpl #f)))]
            [#(,tmpl* ...)
             (let-values ([(out* var*?) (gen-template tmpl* depth)])
               (if var*?
                   (values `(vector ,out* ...) #t)
                   (values `',tmpl #f)))]
            [quasiquote
             (syntax-error who "misplaced quasiquote in template" tmpl)]
            [,uq
             (guard (unquote? uq))
             (syntax-error who "misplaced unquote in template" tmpl)]
            [unquote-splicing
             (syntax-error who "misplaced unquote-splicing in template" tmpl)]
            [,tmpl
             (values `',tmpl #f)])))
      (syntax-match x
        [(,k ,tmpl1)
         (let-values ([(out var?) (gen-template tmpl1 0)])
           (expand-expression out))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax when
    (lambda (x)
      (define who 'when)
      (syntax-match x
        [(,k ,t ,e* ... ,e)
         (expand-expression
          `(if ,t (begin ,e* ... ,e) (values)))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax unless
    (lambda (x)
      (define who 'unless)
      (syntax-match x
        [(,k ,t ,e* ... ,e)
         (expand-expression
          `(if ,t (values) (begin ,e* ... ,e)))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax do
    (lambda (x)
      (define who 'do)
      (define parse-do-clause*
        (lambda (cl*)
          (let f ([cl* cl*]
                  [x* '()]
                  [i* '()]
                  [s* '()])
            (if (null? cl*)
                (begin
                  (unless (valid-bound-identifiers? x*)
                    (syntax-error who "invalid syntax" x))
                  (values x* i* s*))
                (syntax-match (car cl*)
                  [[,x ,i ,s]
                   (guard ($identifier? x))
                   (f (cdr cl*) (cons x x*) (cons i i*) (cons s s*))]
                  [[,x ,i]
                   (guard ($identifier? x))
                   (f (cdr cl*) (cons x x*) (cons i i*) (cons i s*))]
                  [,cl
                   (syntax-error who "invalid do clause" x cl)])))))
      (syntax-match x
        [(,k (,cl* ...) (,t ,e* ...) ,c* ...)
         (let-values ([(x* i* s*) (parse-do-clause* cl*)])
           (let ([f (generate-temporary)])
             (expand-expression
              `(let f [(,x* ,i*) ...]
                 (if ,t
                     (begin (values) ,e* ...)
                     (begin
                       ,c* ...
                       (,f ,s* ...)))))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax syntax-rules
    (lambda (x)
      (define who 'syntax-rules)
      (syntax-match x
        [(,k (,lit* ...) [(,k* . ,p*) ,t*] ...)
         (guard (for-all $identifier? lit*) (for-all $identifier? k*))
         (let ([x (generate-temporary)])
           (expand-expression
            `(lambda (,x)
               (syntax-case ,x ,lit*
                 [(_ . ,p*) #',t*] ...))))]
        [,x (syntax-error who "invalid syntax" x)])))

  (declare-expander-syntax identifier-syntax
    (lambda (stx)
      (define who 'identifier-syntax)
      (let ([x (generate-temporary)]
            [y (generate-temporary)])
        (syntax-match stx
          [(,k ,e)
           (let ([id (generate-temporary)])
             (expand-expression
              `(lambda (,x)
                 (syntax-case ,x ()
                   [,id (identifier? #',id) #',e]
                   [(_ ,y (... ...)) #'(,e ,y (... ...))]))))]
          [(,k [,id ,exp1]
               [(set! ,var ,val) ,exp2])
           (guard ($identifier? id) ($identifier? var))
           (expand-expression
            `(make-variable-transformer
              (lambda (,x)
                (syntax-case ,x (set!)
                  [(set! ,var ,val) #',exp2]
                  [(,id ,y (... ...)) #'(,exp1 ,y (... ...))]
                  [,id (identifier? #',id) #',exp1]))))]
          [,x (syntax-error who "invalid syntax" x)]))))

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
                  [ribs (ribcage x* lbl*)]
                  [b (add-substitutions ribs b)]
                  [e (expand-expression b)])
             (for-each label-kill! lbl*)
             e))]
        [,x (syntax-error who "invalid syntax" x)])))

  ;; prims

  (declare-prim-syntax >= (fxnot 1))
  (declare-prim-syntax <= (fxnot 1))
  (declare-prim-syntax append (fxnot 0))
  (declare-prim-syntax car 1)
  (declare-prim-syntax cdr 1)
  (declare-prim-syntax cons 2)
  (declare-prim-syntax cons* (fxnot 1))
  (declare-prim-syntax eq? 2)
  (declare-prim-syntax equal? 2)
  (declare-prim-syntax values (fxnot 0))
  (declare-prim-syntax void 0)
  (declare-prim-syntax list (fxnot 0))
  (declare-prim-syntax make-variable-transformer 1)
  (declare-prim-syntax map (fxnot 1))
  (declare-prim-syntax memv 2)
  (declare-prim-syntax not 1)
  (declare-prim-syntax identifier? 1)
  (declare-prim-syntax free-identifier=? 2)
  (declare-prim-syntax reverse 1)
  (declare-prim-syntax set-box! 2)
  (declare-prim-syntax unbox 1)
  (declare-prim-syntax syntax-car 1)
  (declare-prim-syntax syntax-cdr 1)
  (declare-prim-syntax syntax-null? 1)
  (declare-prim-syntax syntax-pair? 1)
  (declare-prim-syntax syntax->datum 1)
  (declare-prim-syntax syntax-list 1)
  (declare-prim-syntax syntax-list->vector 1)
  (declare-prim-syntax syntax-split 4)
  (declare-prim-syntax syntax-vector? 1)
  (declare-prim-syntax syntax-vector->list 1)
  (declare-prim-syntax syntax-violation (fxnot 3))

  ;; DEBUG
  (declare-prim-syntax display 1)
  (declare-prim-syntax newline 0)

  )
