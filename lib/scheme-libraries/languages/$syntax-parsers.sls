#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries languages $syntax-parsers)
  (export
    parse-terminals-clause
    parse-nonterminal-clause
    parse-extended-terminals-clause
    parse-extended-nonterminal-clauses)
  (import
    (rnrs)
    (scheme-libraries))

  (define who 'define-language)

  ;; TODO: We can integrate this back into languages.sls.
  ;; TODO: Check metavars (they must not end in digit, *, ^, ?.

  (define parse-terminal-clause
    (lambda (stx cl)
      (syntax-case cl ()
        [(terminal-name (meta-var ...))
         (for-all identifier? #'(terminal-name meta-var ...))
         #'(terminal-name (meta-var ...))]
        [_ (syntax-violation who "invalid terminal clause" stx cl)])))

  (define parse-terminals-clause
    (lambda (stx cl)
      (syntax-case cl ()
        [(_ terminal-clause ...)
         (map (lambda (cl)
                (parse-terminal-clause stx cl))
              #'(terminal-clause ...))]
        [#f
         (syntax-violation who "missing terminals clause" stx)]
        [_ (syntax-violation who "invalid terminals clause" stx cl)])))

  (define parse-nonterminal-clauses
    (lambda (stx cl*)
      (define nonterminals (make-eq-hashtable))
      (define parse-nonterminal-clause
        (lambda (stx cl)
          (syntax-case cl ()
            [(nonterminal-name (meta-var ...) production-clause1 production-clause2 ...)
             (and (for-all identifier? #'(nonterminal-name meta-var ...))
                  ;; XXX: Where to check production clauses?
                  (for-all production-clause? #'(production-clause1 production-clause2 ...)))
             (begin
               (hashtable-update!
                nonterminals
                (syntax->datum #'nonterminal-name)
                (lambda (val)
                  (when val
                    (syntax-violation who "duplicate nonterminal clause" stx cl))
                  #t)
                #f)
               #'(nonterminal-name (meta-var ...) production-clause1 production-clause2 ...))]
            [_ (syntax-violation who "invalid nonterminal clause" stx cl)])))
      (map parse-nonterminal-clause cl*)))

  (define parse-extended-terminals-clause
    (lambda (stx terminals cl)
      (define remove-terminals
        (lambda (minus terminals)
          (fold-left
           (lambda (terminals terminal)
             (let f ([terminals terminals])
               (cond
                [(null? terminals)
                 (syntax-violation who "terminal not defined in base language" terminal)]
                [(terminal=? (car terminals) terminal)
                 (cdr terminals)]
                [else
                 (cons (car terminals) (f (cdr terminals)))])))
           terminals minus)))
      (define add-terminals
        (lambda (plus terminals)
          (append terminals plus)))
      (define parse-extended-terminal-clause
        (lambda (cl plus minus)
          (syntax-case cl (+ -)
            [(+ terminal-clause ...)
             (values
              (append (reverse (map (lambda (cl)
                                      (parse-terminal-clause stx cl))
                                    #'(terminal-clause ...)))
                      plus)
              minus)]
            [(- terminal-clause ...)
             (values
              plus
              (append (reverse (map (lambda (cl)
                                      (parse-terminal-clause stx cl))
                                    #'(terminal-clause ...)))
                      minus))]
            [_ (syntax-violation who "invalid extended terminal clause" stx cl)])))
      (syntax-case cl ()
        [(_ extended-terminal-clause ...)
         (let f ([cl* #'(extended-terminal-clause ...)] [plus '()] [minus '()])
           (if (null? cl*)
               (add-terminals (reverse plus) (remove-terminals (reverse minus) terminals))
               (let ([cl (car cl*)] [cl* (cdr cl*)])
                 (let-values ([(plus minus)
                               (parse-extended-terminal-clause cl plus minus)])
                   (f cl* plus minus)))))])))

  (define terminal=?
    (lambda (t1 t2)
      (equal? (syntax->datum t1) (syntax->datum t2))))

  (define nonterminal=?
    (lambda (n1 n2)
      (equal? (syntax->datum n1) (syntax->datum n2))))

  (define parse-extended-nonterminal-clauses
    (lambda (stx nonterminals cl*)

      (define remove-production-clauses
        (lambda (minus production-clauses)
          (fold-left
           (lambda (base-production-clauses production-clause)
             (let f ([base-production-clauses base-production-clauses])
               (cond
                [(null? base-production-clauses)
                 (syntax-violation who "production clause not defined in base language" production-clause)]
                [(production-clause=? (car base-production-clauses) production-clause)
                 (cdr base-production-clauses)]
                [else
                 (cons (car base-production-clauses)
                       (f (cdr base-production-clauses)))])))
           #'(base-production-clauses ...) minus)))

      (define remove-nonterminals
        (lambda (name meta-var* minus nonterminals)
          (let f ([nonterminals nonterminals])
            (syntax-case nonterminals ()
              [()
               (syntax-violation who "nonterminal not defined in base language" stx name)]
              [(((base-nonterminal-name (base-meta-var ...)) base-production-clause ...) . nonterminals)
               (symbolic-identifier=? name #'base-nonterminal-name)
               (let ([production-clause* (remove-production-clauses minus #'(base-production-clause ...))])
                 (if (null? production-clause*)
                     #'nonterminals
                     (cons (cons* name meta-var* production-clause*) #'nonterminals)))]
              [(nonterminal . nonterminals)
               (cons #'nonterminal (f #'nonterminals))]))))

      (define add-nonterminals
        (lambda (name meta-var* plus nonterminals)
          (let f ([nonterminals nonterminals])
            (syntax-case nonterminals ()
              [()
               (list (cons* name meta-var* plus))]
              [(((base-nonterminal-name (base-meta-var ...)) base-production-clause ...) . nonterminals)
               (symbolic-identifier=? name #'base-nonterminal-name)
               (cons (cons* name meta-var* (append #'(base-production-clause ...) plus))
                     #'nonterminals)]
              [(nonterminal . nonterminals)
               (cons #'nonterminal (f #'nonterminals))]))))

      (define parse-extended-production-clause
        (lambda (nonterminal cl plus minus)
          (syntax-case cl (+ -)
            [(+ production-clause ...)
             ;; TODO: Where to syntax-check production clause?
             (for-all production-clause? #'(production-clause ...))
             (values
              (append (reverse #'(production-clause ...)) plus)
              minus)]
            [(- production-clause ...)
             (for-all production-clause? #'(production-clause ...))
             (values
              plus
              (append (reverse #'(production-clause ...)) minus))]
            [_ (syntax-violation who "invalid extended production clause" stx cl)])))
      (define nonterminal-names (make-eq-hashtable))
      (define parse-extended-nonterminal-clause
        (lambda (nonterminals cl)
          (syntax-case cl ()
            [(nonterminal-name (meta-var ...) extended-production-clause ...)
             (for-all identifier? #'(nonterminal-name meta-var ...))
             (let ([nonterminal-name #'nonterminal-name])
               (hashtable-update!
                nonterminal-names
                (syntax->datum nonterminal-name)
                (lambda (val)
                  (when val
                    (syntax-violation who "duplicate extended nonterminal clause" stx cl))
                  #t)
                #f)
               (let f ([cl* #'(extended-production-clause ...)]
                       [plus '()]
                       [minus '()])
                 (if (null? cl*)
                     (add-nonterminals nonterminal-name #'(meta-var ...)
                                       (reverse plus)
                                       (remove-nonterminals nonterminal-name #'(meta-var ...)
                                                            (reverse minus)
                                                            nonterminals))
                     (let ([cl (car cl*)] [cl* (cdr cl*)])
                       (let-values ([(plus minus)
                                     (parse-extended-production-clause cl plus minus)])
                         (f cl* plus minus))))))]
            [_ (syntax-violation who "invalid extended nonterminal clause")])))
      (fold-left parse-extended-nonterminal-clause nonterminals cl*)))

  (define production-clause?
    (lambda (cl)
      ;; FIXME
      #t)))
