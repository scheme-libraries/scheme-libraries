#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries r7rs)
  (export
    syntax-rules
    syntax-error)
  (import
    (rename (rnrs)
      (syntax-rules r6rs:syntax-rules))
    (scheme-libraries define-who)
    (scheme-libraries quote-syntax))

  ;; syntax-rules

  (define-syntax/who syntax-rules
    (lambda (stx)

      ;; Test whether an identifier is default ellipsis.
      (define ellipsis?
        (lambda (id)
          (free-identifier=? id #'(... ...))))

      ;; Test whether an identifier is the default underscore.
      (define underscore?
        (lambda (id)
          (free-identifier=? id #'_)))

      ;; Collect the identifiers appearing in a pattern in a list.
      (define flatten
        (lambda (pat)
          (syntax-case pat ()
            [(x . y)
             `(,@(flatten #'x) ,@(flatten #'y))]
            [#(x ...)
             (flatten #'(x ...))]
            [x
             (identifier? #'x)
             `(,#'x)]
            [_ '()])))

      ;; Transform the literals, patterns and templates and generate
      ;; an equivalent syntax-case expression.
      (define transform
        (lambda (ell lit* pat* tmpl*)

          ;; Test whether a form is an identifier representing the
          ;; custom ellipsis.
          (define custom-ellipsis?
            (lambda (stx)
              (and ell
                   (identifier? stx)
                   (bound-identifier=? stx ell))))

          ;; Test whether literal has to be removed from the list of
          ;; literals.
          (define special-literal?
            (lambda (id)
              (or (ellipsis? id)
                  (underscore? id)
                  (custom-ellipsis? id))))

          ;; Test whether a form acts as the actual ellipsis.
          (define actual-ellipsis?
            (let ([test
                   (if ell
                       (if (find custom-ellipsis? lit*) (lambda (x) #f) custom-ellipsis?)
                       (if (find ellipsis? lit*) (lambda (x) #f) ellipsis?))])
              (lambda (stx)
                (and (identifier? stx)
                     (test stx)))))

          ;; Replace special literal in patterns by fresh ones and add
          ;; literal tests to collections of guards.
          (define replace-literal*
            (lambda (lit pat* guard**)
              (define replace-literal
                (lambda (pat guard*)
                  (let-values ([(pat guards)
                                (replace-in-pattern lit pat)])
                    (values pat
                            #`(#,@guard* #,@guards)))))
              (let f ([pat* pat*] [guard** guard**])
                (if (null? pat*)
                    (values '() '())
                    (let-values ([(pat guard*)
                                  (replace-literal (car pat*) (car guard**))]
                                 [(pat* guard**)
                                  (f (cdr pat*) (cdr guard**))])
                      (values (cons pat pat*)
                              (cons guard* guard**)))))))

          ;; Replace each occurrence of an identifier in a pattern by
          ;; a fresh one and add a literal test to a collection of
          ;; guards.
          (define replace-in-pattern
            (lambda (id pat)
              (let f ([pat pat] [depth #'()])
                (syntax-case pat ()
                  [(x . y)
                   (let g ([y #'y] [ells #'()] [depthx depth])
                     (syntax-case y ()
                       [(::: . y)
                        ;; XXX
                        (actual-ellipsis? #':::)
                        (g #'y #`(#,@ells :::) #`((... ...) #,@depthx))]
                       [y
                        (let-values ([(x guardsx) (f #'x depthx)]
                                     [(y guardsy) (f #'y depth)])
                          (values #`(#,x #,@ells . #,y)
                                  #`(#,@guardsx #,@guardsy)))]))]
                  [#(x ...)
                   (let-values ([(x* guards) (f #'(x ...) depth)])
                     (values #`#(#,@x*) guards))]
                  [x
                   (identifier? #'x)
                   (if (bound-identifier=? #'x id)
                       (with-syntax ([(tmp) (generate-temporaries #'(tmp))])
                         (values #'tmp
                                 #`((for-all (lambda (e)
                                               (and (identifier? e)
                                                    (free-identifier=? e (quote-syntax #,id))))
                                             #'(tmp #,@depth)))))
                       (values #'x '()))]
                  [x (values #'x '())]))))

          ;; Apply substitions to a list of a patterns
          (define replace-pattern-variables-in-pattern*
            (lambda (substs pat*)
              (map (lambda (pat)
                     (replace-pattern-variables-in-pattern substs pat))
                   pat*)))

          ;; Apply substitutions to a list of templates
          (define replace-pattern-variables-in-template*
            (lambda (substs tmpl*)
              (map (lambda (tmpl)
                     (replace-pattern-variables-in-template substs tmpl))
                   tmpl*)))

          ;; Apply substitutions to a pattern
          (define replace-pattern-variables-in-pattern
            (lambda (substs pat)
              (let f ([pat pat])
                (syntax-case pat ()
                  [(x . y)
                   #`(#,(f #'x) . #,(f #'y))]
                  [#(x ...)
                   #`#(#,@(map f #'(x ...)))]
                  [x
                   (identifier? #'x)
                   (cond
                    [(assp (lambda (y)
                             (bound-identifier=? #'x y))
                           substs)
                     => cdr]
                    [else #'x])]
                  [x #'x]))))

          ;; Apply substitutions to a template, respecting escaped
          ;; subtemplates.
          (define replace-pattern-variables-in-template
            (lambda (substs pat)
              (let f ([pat pat])
                (syntax-case pat ()
                  [(::: x)
                   (custom-ellipsis? #':::) pat]
                  [(x . y)
                   #`(#,(f #'x) . #,(f #'y))]
                  [#(x ...)
                   #`#(#,@(map f #'(x ...)))]
                  [x
                   (identifier? #'x)
                   (cond
                    [(assp (lambda (y)
                             (bound-identifier=? #'x y))
                           substs)
                     => cdr]
                    [else #'x])]
                  [x #'x]))))

          ;; Replace all pattern variables that must not be
          ;; interpreted as an ellipsis in a list of patterns and
          ;; templates.
          (define replace-default-ellipses*
            (lambda (pat* tmpl*)
              (if (null? pat*)
                  (values '() '())
                  (let-values ([(pat tmpl) (replace-default-ellipses (car pat*) (car tmpl*))]
                               [(pat* tmpl*) (replace-default-ellipses* (cdr pat*) (cdr tmpl*))])
                    (values (cons pat pat*)
                            (cons tmpl tmpl*))))))

          ;; Replace all pattern variables that must not be
          ;; interpreted as an ellipsis.
          (define replace-default-ellipses
            (lambda (pat tmpl)
              (let* ([ell* (filter (lambda (x)
                                     (and (ellipsis? x)
                                          (not (custom-ellipsis? x))))
                                   (flatten pat))]
                     [tmp* (generate-temporaries ell*)]
                     [substs (map cons ell* tmp*)])
                (values (replace-pattern-variables-in-pattern substs pat)
                        (replace-pattern-variables-in-template substs tmpl)))))

          ;; Escape default ellipses and replace custom ellipses
          ;; by default ellipses in a template.
          (define replace-ellipsis-in-template
            (lambda (tmpl)
              (let f ([tmpl tmpl])
                (syntax-case tmpl ()
                  [(::: x)
                   (custom-ellipsis? #':::)
                   #'((... ...) x)]
                  [(x . y)
                   (let g ([y #'y] [ells #'()])
                     (syntax-case y ()
                       [(::: . y)
                        (custom-ellipsis? #':::)
                        (g #'y #`(#,@ells (... ...)))]
                       [y
                        #`(#,(f #'x) #,@ells . #,(f #'y))]))]
                  [(x . y)
                   #`(#,(f #'x) . #,(f #'y))]
                  [#(x ...)
                   #`#(#,@(map f #'(x ...)))]
                  [x
                   (identifier? #'x)
                   (if (ellipsis? #'x)
                       #'((... ...) x)
                       #'x)]
                  [x #'x]))))

          ;; Replace any identifier corresponding to a special literal
          ;; in any pattern by a generated one with explicit guards.
          (define replace-special-literals
            (lambda (pat*)
              (let ([guard** (map (lambda (pat) '()) pat*)])
                (let f ([lit* lit*])
                  (if (null? lit*)
                      (values '()  pat* guard**)
                      (let ([lit (car lit*)]
                            [lit* (cdr lit*)])
                        (let-values ([(lit* pat* guard**) (f lit*)])
                          (if (special-literal? lit)
                              (let-values ([(pat* guard**) (replace-literal* lit pat* guard**)])
                                (values lit* pat* guard**))
                              (values (cons lit lit*) pat* guard**)))))))))

          (let-values ([(lit* pat* guard**) (replace-special-literals pat*)])
            (if ell
                ;; With custom ellipsis.
                (let  ([pat*
                        ;; Normalize literals in patterns so that none
                        ;; is a special literal.
                        (replace-pattern-variables-in-pattern* (map cons lit* lit*) pat*)])
                  (let-values ([(pat* tmpl*) (replace-default-ellipses* pat* tmpl*)])
                    (let ([pat* (replace-pattern-variables-in-pattern* `((,ell . ,#'(... ...))) pat*)]
                          [tmpl* (map replace-ellipsis-in-template tmpl*)])
                      (generate lit* pat* guard** tmpl*))))
                ;; Without custom ellipsis.
                (generate lit* pat* guard** tmpl*)))))

      ;; Generate the output syntax-case form equivalent to the input
      ;; syntax-rules form.
      (define generate
        (lambda (lit* pat* guard** tmpl*)
          (with-syntax ([(lit ...) lit*]
                        [(pat ...) pat*]
                        [((guard ...) ...) guard**]
                        [(tmpl ...) tmpl*])
            #'(lambda (stx)
                (syntax-case stx (lit ...)
                  [(_ . pat)
                   (and guard ...)
                   #'tmpl]
                  ...)))))

      ;; Parse syntax-rules form.
      (syntax-case stx ()
        ;; (syntax-rules (<literal> ...) <clause> ...)
        [(_ (lit ...) [(k . pat) tmpl] ...)
         (for-all identifier? #'(k ... lit ...))
         (transform #f #'(lit ...) #'(pat ...) #'(tmpl ...))]
        ;; (syntax-rules <ellipsis> (<literal> ...) <clause> ...)
        [(_ ell (lit ...) [(k . pat) tmpl] ...)
         (for-all identifier? #'(ell k ... lit ...))
         (transform #'ell #'(lit ...) #'(pat ...) #'(tmpl ...))]
        [_ (syntax-violation who "invalid syntax" stx)])))

  ;; syntax-error

  (define-syntax/who syntax-error
    (lambda (stx)
      (syntax-case stx ()
        [(k msg arg ...)
         (raise (condition
                  (make-who-condition who)
                  (make-message-condition (syntax->datum #'msg))
                  (make-syntax-violation #'k #f)
                  (make-irritants-condition #'(arg ...))))]
        [_ (syntax-violation who "invalid syntax" stx)]))))
