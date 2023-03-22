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

  (define parse-nonterminal-clause
    (lambda (stx cl)
      (syntax-case cl ()
        [(nonterminal-name (meta-var ...) production-clause1 production-clause2 ...)
         (for-all identifier? #'(nonterminal-name meta-var ...))
         ;; FIXME: Check whether prod-clause IS a prod-clause!
         #'(nonterminal-name (meta-var ...) production-clause1 production-clause2 ...)])))

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
      (define remove-nonterminals
        (lambda (minus nonterminals)
          (fold-left
           (lambda (nonterminals nonterminal-clause)
             ;; FIXME: At this point, the nonterminal-clause is (extended-clause . production-rule)
             (let f ([nonterminals nonterminals])
               (if (null? f)
                   (syntax-violation who "nonterminal not defined in base language" ...)

                   )
               )
             (let f ([nonterminals nonterminals])
               (syntax-case nonterminals ()
                 [()
                  (syntax-violation who "nonterminal not defined in base language" stx (car nonterminal-clause))]
                 [(((base-nonterminal (base-meta-var ...)) base-production-clause ...) . nonterminals)
                  (nonterminal=? #'base-nonterminal #'nonterminal)
                  ;; TODO: Remove production rule
                  (assert #f)]
                 [(_ . nonterminals)
                  (f #'nonterminals)])))
           nonterminals minus)))
      (define add-nonterminals
        (lambda (plus nonterminals)
          (fold-left
           (lambda (nonterminals nonterminal-clause)
             (with-syntax ([(nonterminal production-clause)
                            nonterminal-clause])
               (let f ([nonterminals nonterminals])
                 (syntax-case nonterminals ()
                   [()
                    (syntax-violation who "nonterminal not defined in base language" stx (car #'nonterminal))]
                   [((base-nonterminal base-production-clause ...) . nonterminals)
                    (cons #'(base-nonterminal base-production-clause ... production-clause)
                          #'nonterminals)]
                   [(_ . nonterminals)
                    (f #'nonterminals)]))))
           nonterminals plus)))
      (define parse-extended-nonterminal-clause
        (lambda (cl plus minus)
          (syntax-case cl ()
            [(nonterminal-name (mvar ...) extended-production-clause ...)
             (for-all identifier? #'(nonterminal-name mvar ...))
             (let f ([cl* #'(extended-production-clause ...)]
                     [plus plus]
                     [minus minus])
               (if (null? cl*)
                   (values plus minus)
                   (let ([production-cl (car cl*)] [cl* (cdr cl*)])
                     (let-values ([(plus minus)
                                   (parse-extended-production-clause cl production-cl plus minus)])
                       (f cl* plus minus)))))]
            [_ (syntax-violation who "invalid extended nonterminal clause" stx cl)])))
      (define parse-extended-production-clause
        (lambda (nonterminal cl plus minus)
          (syntax-case cl (+ -)
            [(+ production-clause ...)
             (for-all production-clause? #'(production-clause ...))
             (values
              (append (reverse (map (lambda (cl)
                                      (list nonterminal cl))
                                    #'(production-clause ...)))
                      plus)
              minus)]
            [(- production-clause ...)
             (for-all production-clause? #'(production-clause ...))
             (values
              plus
              (append (reverse (map (lambda (cl)
                                      (list nonterminal cl))
                                    #'(production-clause ...)))
                      minus))]
            [_ (syntax-violation who "invalid extended production clause" stx cl)])))
      (let f ([cl* cl*] [plus '()] [minus '()])
        (if (null? cl*)
            (add-nonterminals (reverse plus) (remove-nonterminals (reverse minus) nonterminals))
            (let ([cl (car cl*)] [cl* (cdr cl*)])
              (let-values ([(plus minus)
                            (parse-extended-nonterminal-clause cl plus minus)])
                (f cl* plus minus)))))))

  (define production-clause?
    (lambda (cl)
      ;; FIXME
      #t)))
