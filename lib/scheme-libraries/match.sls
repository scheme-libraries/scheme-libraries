#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries match)
  (export
    match
    unquote
    ...
    _
    ->
    guard
    extend-backquote)
  (import
    (rnrs)
    (scheme-libraries define-auxiliary-syntax)
    (scheme-libraries define-who)
    (scheme-libraries helpers)
    (scheme-libraries lists)
    (scheme-libraries with-implicit))

  (define-auxiliary-syntax ->)

  (define split
    (lambda (obj k succ fail)
      (let ([n (length+ obj)])
        (if (and n
                 (fx<=? k n))
            (call-with-values
                (lambda ()
                  (split-at obj (fx- n k)))
              succ)
            (fail)))))

  (define-syntax/who match
    (lambda (stx)
      (define-record-type pattern-variable
        (nongenerative) (sealed #t) (opaque #t)
        (fields (mutable identifier) expression level))
      (define-record-type cata-binding
        (nongenerative) (sealed #t) (opaque #t)
        (fields proc-expr value-id* identifier))
      (define make-identifier-hashtable
	(lambda ()
	  (define identifier-hash
	    (lambda (id)
	      (assert (identifier? id))
	      (symbol-hash (syntax->datum id))))
	  (make-hashtable identifier-hash bound-identifier=?)))
      (define pattern-variable-guards
	(lambda (pvars)
	  (define ht (make-identifier-hashtable))
          (fold-left
           (lambda (guards pvar)
             (let ([id (pattern-variable-identifier pvar)])
               (cond
                [(hashtable-ref ht id #f)
                 (with-syntax ([id id]
                               [(new-id) (generate-temporaries #'(id))]
                               [guards guards])
                   (pattern-variable-identifier-set! pvar #'new-id)
                   #'((equal? id new-id) . guards))]
                [else
                 (hashtable-set! ht id #t)
                 guards])))
           '() pvars)))
      (define check-cata-bindings
	(lambda (catas)
	  (define ht (make-identifier-hashtable))
	  (for-each
	   (lambda (cata)
	     (for-each
	      (lambda (id)
		(hashtable-update!
                 ht
		 id
		 (lambda (val)
		   (when val
		     (syntax-violation who "repeated cata variable in match clause" stx id))
		   #t)
		 #f))
	      (cata-binding-value-id* cata)))
	   catas)))
      (define parse-clause
        (lambda (cl)
          (syntax-case cl (guard)
            [(pat (guard guard-expr ...) e1 e2 ...)
             (values #'pat #'(and guard-expr ...) #'(e1 e2 ...))]
            [(pat e1 e2 ...)
             (values #'pat #'#t #'(e1 e2 ...))]
            [_
             (syntax-violation who "ill-formed match clause" stx cl)])))
      (define gen-matcher
        (lambda (expr pat)
          (define ill-formed-match-pattern-violation
            (lambda ()
              (syntax-violation who "ill-formed match pattern" stx pat)))
          (syntax-case pat (-> unquote)
            [,[f -> y ...]
             (for-all identifier? #'(y ...))
             (with-syntax ([(x) (generate-temporaries '(x))])
               (values
                (lambda (k)
                  (k))
                (list (make-pattern-variable #'x expr 0))
                (list (make-cata-binding #'f #'(y ...) #'x))))]
            [,[y ...]
             (for-all identifier? #'(y ...))
             (with-syntax ([(x) (generate-temporaries '(x))])
               (values
                (lambda (k)
                  (k))
                (list (make-pattern-variable #'x expr 0))
                (list (make-cata-binding #'loop #'(y ...) #'x))))]
            [(pat1 ell pat2 ... . ,e)
             (ellipsis? #'ell)
             (gen-ellipsis-matcher expr #'pat1 #'(pat2 ...) #',e)]
            [(pat1 ell pat2 ... . pat3)
             (ellipsis? #'ell)
             (gen-ellipsis-matcher expr #'pat1 #'(pat2 ...) #'pat3)]
            [#(x ...)
             (with-syntax ([(e) (generate-temporaries '(e))])
               (let-values ([(mat pvars catas)
                             (gen-matcher #'e #'(x ...))])
                 (values
                  (lambda (k)
                    #`(if (vector? #,expr)
                          (let ([e (vector->list #,expr)])
                            #,(mat k))
                          (fail)))
                  pvars catas)))]
            [,x
             (identifier? #'x)
             (values
              (lambda (k)
                (k))
              (list (make-pattern-variable #'x expr 0))
              '())]
            [(pat1 . pat2)
             (with-syntax ([(e1 e2) (generate-temporaries '(e1 e2))])
               (let*-values ([(mat1 pvars1 catas1)
                              (gen-matcher #'e1 #'pat1)]
                             [(mat2 pvars2 catas2)
                              (gen-matcher #'e2 #'pat2)])
                 (values
                  (lambda (k)
                    #`(if (pair? #,expr)
                          (let ([e1 (car #,expr)]
                                [e2 (cdr #,expr)])
                            #,(mat1 (lambda () (mat2 k))))
                          (fail)))
                  (append pvars1 pvars2) (append catas1 catas2))))]
            [unquote
             (ill-formed-match-pattern-violation)]
            [_
             (values
              (lambda (k)
                #`(if (equal? #,expr '#,pat)
                      #,(k)
                      (fail)))
              '() '())])))
      (define gen-ellipsis-matcher
        (lambda (expr pat1 pat2* pat3)
          (with-syntax ([(e1 e2) (generate-temporaries '(e1 e2))])
            (let*-values ([(mat1 pvars1 catas1)
                           (gen-map #'e1 pat1)]
                          [(mat2 pvars2 catas2)
                           (gen-matcher* #'e2 (append pat2* pat3))])
              (values
               (lambda (k)
                 #`(split
                    #,expr
                    #,(length pat2*)
                    (lambda (e1 e2)
                      #,(mat1 (lambda () (mat2 k))))
                    fail))
               (append pvars1 pvars2)
               (append catas1 catas2))))))
      (define gen-matcher*
        (lambda (expr pat*)
          (syntax-case pat* (unquote)
            [()
             (values
              (lambda (k)
                #`(if (null? #,expr)
                      #,(k)
                      (fail)))
              '() '())]
            [,x
             (gen-matcher expr pat*)]
            [(pat . pat*)
             (with-syntax ([(e1 e2) (generate-temporaries '(e1 e2))])
               (let*-values ([(mat1 pvars1 catas1)
                              (gen-matcher #'e1 #'pat)]
                             [(mat2 pvars2 catas2)
                              (gen-matcher* #'e2 #'pat*)])
                 (values
                  (lambda (k)
                    #`(let ([e1 (car #,expr)]
                            [e2 (cdr #,expr)])
                        #,(mat1
                           (lambda ()
                             (mat2 k)))))
                  (append pvars1 pvars2)
                  (append catas1 catas2))))]
            [_
             (gen-matcher expr pat*)])))
      (define gen-map
        (lambda (expr pat)
          (with-syntax ([(e1 e2 f) (generate-temporaries '(e1 e2 f))])
            (let-values ([(mat ipvars catas)
                          (gen-matcher #'e1 pat)])
              (with-syntax ([(u ...)
                             (generate-temporaries ipvars)]
                            [(v ...)
                             (map pattern-variable-expression ipvars)])
                (values
                 (lambda (k)
                   #`(let f ([e2 (reverse #,expr)]
                             [u '()] ...)
                       (if (null? e2)
                           #,(k)
                           (let ([e1 (car e2)])
                             #,(mat (lambda ()
                                      #`(f (cdr e2) (cons v u) ...)))))))
                 (map
                  (lambda (id pvar)
                    (make-pattern-variable
                     (pattern-variable-identifier pvar)
                     id
                     (fx+ (pattern-variable-level pvar) 1)))
                  #'(u ...) ipvars)
                 catas))))))
      (define gen-map-values
        (lambda (proc-expr y* e n)
          (let f ([n n])
            (if (fxzero? n)
                #`(#,proc-expr #,e)
                (with-syntax ([(tmps ...)
                               (generate-temporaries y*)]
                              [(tmp ...)
                               (generate-temporaries y*)]
                              [e e])
                  #`(let f ([e* (reverse e)]
                            [tmps '()] ...)
                      (if (null? e*)
                          (values tmps ...)
                          (let ([e (car e*)]
                                [e* (cdr e*)])
                            (let-values ([(tmp ...)
                                          #,(f (fx- n 1))])
                              (f e* (cons tmp tmps) ...))))))))))
      (define gen-clause
        (lambda (k cl)
          (let*-values ([(pat guard-expr body)
                         (parse-clause cl)]
                        [(matcher pvars catas)
                         (gen-matcher #'e pat)])
            (define pvar-guards (pattern-variable-guards pvars))
	    (check-cata-bindings catas)
            (with-syntax ([(x ...)
                           (map pattern-variable-identifier pvars)]
                          [(u ...)
                           (map pattern-variable-expression pvars)]
                          [(f ...)
                           (map cata-binding-proc-expr catas)]
                          [((y ...) ...)
                           (map cata-binding-value-id* catas)]
                          [(z ...)
                           (map cata-binding-identifier catas)]
                          [(tmp ...)
                           (generate-temporaries catas)])
              (with-syntax ([(e ...)
                             (map
                              (lambda (tmp y* z)
                                (let ([n
                                       (exists
                                        (lambda (pvar)
                                          (let ([x (pattern-variable-identifier pvar)])
                                            (and (bound-identifier=? x z)
                                                 (pattern-variable-level pvar))))
                                        pvars)])
                                  (gen-map-values tmp y* z n)))
                              #'(tmp ...) #'((y ...) ...) #'(z ...))])
                (matcher
                 (lambda ()
                   #`(let ([x u] ...)
                       (if (and #,@pvar-guards #,guard-expr)
                           (let ([tmp f] ...)
                             (let-values ([(y ...) e] ...)
                               (extend-backquote #,k
                                 #,@body)))
                           (fail))))))))))
      (define gen-match
        (lambda (k cl*)
          (fold-right
           (lambda (cl rest)
             #`(let ([fail (lambda () #,rest)])
                 #,(gen-clause k cl)))
           #'(assertion-violation 'match "expression does not match" e)
           cl*)))
      (syntax-case stx ()
        [(k expr cl ...)
         #`(let loop ([e expr])
             #,(gen-match #'k #'(cl ...)))])))

  (define-syntax/who extend-backquote
    (lambda (x)
      (syntax-case x ()
        [(_ here e1 ... e2)
         (identifier? #'here)
         (with-implicit (here quasiquote)
           #'(let-syntax ([quasiquote quasiquote-transformer])
               e1 ... e2))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define quasiquote-transformer
    (lambda (stx)
      (define who 'quasiquote)
      (define-record-type template-variable
        (nongenerative) (sealed #t) (opaque #t)
        (fields identifier expression))
      (define quasiquote-syntax-violation
        (lambda (subform msg)
          (syntax-violation 'quasiquote msg stx subform)))
      (define gen-output
        (lambda (k tmpl lvl ell?)
          (define quasiquote?
            (lambda (x)
              (and (identifier? x) (free-identifier=? x k))))
          (define gen-ellipsis
            (lambda (tmpl* out* vars* depth tmpl2)
              (let f ([depth depth] [tmpl2 tmpl2])
                (syntax-case tmpl2 ()
                  [(ell . tmpl2)
                   (ell? #'ell)
                   (f (fx+ depth 1) #'tmpl2)]
                  [tmpl2
                   (let-values ([(out2 vars2)
                                 (gen-output k #'tmpl2 0 ell?)])
		     (for-each
		      (lambda (tmpl vars)
			(when (null? vars)
			  (quasiquote-syntax-violation tmpl "no substitutions to repeat here")))
		      tmpl* vars*)
                     (with-syntax ([((tmp** ...) ...)
                                    (map (lambda (vars)
					   (map template-variable-identifier vars))
					 vars*)]
				   [(out1 ...) out*])
                       (values #`(append (append-n-map #,depth
						       (lambda (tmp** ...)
							 out1)
						       tmp** ...)
					 ...
                                         #,out2)
                               (append (apply append vars*) vars2))))]))))
	  (define gen-unquote*
	    (lambda (expr*)
              (with-syntax ([(tmp* ...) (generate-temporaries expr*)])
		(values #'(tmp* ...)
			(map (lambda (tmp expr)
			       (list (make-template-variable tmp expr)))
			     #'(tmp* ...) expr*)))))
	  (syntax-case tmpl (unquote unquote-splicing) ;qq is K.
            ;; (<ellipsis> <template>)
            [(ell tmpl)
             (ell? #'ell)
             (gen-output k #'tmpl lvl (lambda (x) #f))]
            ;; (quasiquote <template>)
            [`tmpl
             (quasiquote? #'quasiquote)
             (let-values ([(out vars) (gen-output k #'tmpl (fx+ lvl 1) ell?)])
               (if (null? vars)
                   (values #'`tmpl
                           '())
                   (values #`(list 'quasiquote #,out)
                           vars)))]
            ;; (unquote <template>)
            [,expr
             (fxzero? lvl)
             (with-syntax ([(tmp) (generate-temporaries '(tmp))])
               (values #'tmp (list (make-template-variable #'tmp #'expr))))]
            [,tmpl
             (let-values ([(out vars)
                           (gen-output k #'tmpl (fx- lvl 1) ell?)])
               (if (null? vars)
                   (values #'',tmpl '())
                   (values #`(list 'unquote #,out) vars)))]
            ;; ((unquote-splicing <template> ...) <ellipsis> . <template>)
	    [((unquote-splicing expr ...) ell . tmpl2)
	     (and (fxzero? lvl) (ell? #'ell))
             (let-values ([(out* vars*)
			   (gen-unquote* #'(expr ...))])
	       (gen-ellipsis #'(expr ...) out* vars* 1 #'tmpl2))]
            ;; (<template> <ellipsis> . <template>)
	    [((unquote expr ...) ell . tmpl2)
	     (and (fxzero? lvl) (ell? #'ell))
             (let-values ([(out* vars*)
			   (gen-unquote* #'(expr ...))])
	       (gen-ellipsis #'(expr ...) out* vars* 0 #'tmpl2))]
            [(tmpl1 ell . tmpl2)
             (and (fxzero? lvl) (ell? #'ell))
             (let-values ([(out1 vars1)
                           (gen-output k #'tmpl1 0 ell?)])
               (gen-ellipsis #'(tmpl1) (list out1) (list vars1) 0 #'tmpl2))]
            ;; ((unquote <template> ...) . <template>)
            [((unquote tmpl1 ...) . tmpl2)
             (let-values ([(out vars)
                           (gen-output k #'tmpl2 lvl ell?)])
               (if (fxzero? lvl)
                   (with-syntax ([(tmp ...)
                                  (generate-temporaries #'(tmpl1 ...))])
                     (values #`(cons* tmp ... #,out)
                             (append
                              (map make-template-variable #'(tmp ...) #'(tmpl1 ...))
                              vars)))
                   (let-values ([(out* vars*)
                                 (gen-output* k #'(tmpl1 ...) (fx- lvl 1) ell?)])
                     (if (and (null? vars)
                              (null? vars*))
                         (values #''((unquote-splicing tmpl1 ...) . tmpl2)
                                 '())
                         (values #`(cons (list 'unquote #,@out*) #,out)
                                 (append vars* vars))))))]

            ;; ((unquote-splicing <template> ...) . <template>)
            [((unquote-splicing tmpl1 ...) . tmpl2)
             ;; TODO: Use gen-ellipsis.
             (let-values ([(out vars)
                           (gen-output k #'tmpl2 lvl ell?)])
               (if (fxzero? lvl)
                   (with-syntax ([(tmp ...)
                                  (generate-temporaries #'(tmpl1 ...))])
                     (values #`(append tmp ... #,out)
                             (append
                              (map make-template-variable #'(tmp ...) #'(tmpl1 ...))
                              vars)))
                   (let-values ([(out* vars*)
                                 (gen-output* k #'(tmpl1 ...) (fx- lvl 1) ell?)])
                     (if (and (null? vars)
                              (null? vars*))
                         (values #''((unquote-splicing tmpl1 ...) . tmpl2)
                                 '())
                         (values #`(cons (list 'unquote-splicing #,@out*) #,out)
                                 (append vars* vars))))))]
            ;; (<element> . <element>)
            [(el1 . el2)
             (let-values ([(out1 vars1)
                           (gen-output k #'el1 lvl ell?)]
                          [(out2 vars2)
                           (gen-output k #'el2 lvl ell?)])
               (if (and (null? vars1)
                        (null? vars2))
                   (values #''(el1 . el2)
                           '())
                   (values #`(cons #,out1 #,out2)
                           (append vars1 vars2))))]
            ;; #(<element> ...)
            [#(el ...)
             (let-values ([(out vars)
                           (gen-output k #'(el ...) lvl ell?)])
               (if (null? vars)
                   (values #'#(el ...) '())
                   (values #`(list->vector #,out) vars)))]
            ;; <constant>
            [constant
             (values #''constant '())])))
      (define gen-output*
        (lambda (k tmpl* lvl ell?)
          (let f ([tmpl* tmpl*] [out* '()] [vars* '()])
            (if (null? tmpl*)
                (values (reverse out*) vars*)
                (let ([tmpl (car tmpl*)]
                      [tmpl* (cdr tmpl*)])
                  (let-values ([(out vars) (gen-output k tmpl lvl ell?)])
                    (f tmpl* (cons out out*) (append vars vars*))))))))
      (syntax-case stx ()
        [(k tmpl)
         (let-values ([(out vars)
                       (gen-output #'k #'tmpl 0 ellipsis?)])
           (with-syntax ([(x ...) (map template-variable-identifier vars)]
                         [(e ...) (map template-variable-expression vars)])
             #`(let ([x e] ...)
                 #,out)))]
        [_
         (syntax-violation who "invalid syntax" stx)])))

  (define append-n-map
    (lambda (n proc . arg*)
      (let f ([n n] [arg* arg*])
        (if (fxzero? n)
            (apply map proc arg*)
            (let ([n (fx- n 1)])
              (apply append
                     (apply map
                            (lambda arg*
                              (f n arg*))
                            arg*)))))))


  )

;; Local Variables:
;; mode: scheme
;; End:
