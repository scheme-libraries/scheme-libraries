#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries languages)
  (export
    define-language
    +
    -
    entry
    extends
    maybe
    terminals
    language->datum)
  (import
    (rnrs)
    (scheme-libraries)
    (scheme-libraries languages $syntax-parsers))

  (define-auxiliary-syntax extends)
  (define-auxiliary-syntax entry)
  (define-auxiliary-syntax maybe)
  (define-auxiliary-syntax terminals)

  (define language->datum-key)
  (define extend-language-key)

  (define-syntax/who define-language
    (lambda (x)
      (define parse-clauses
        (lambda (cl*)
          (let f ([cl* cl*]
                  [extension-clause #f]
                  [entry-clause #f]
                  [terminals-clause #f]
                  [nonterminal-clauses '()])
            (if (null? cl*)
                (values extension-clause
                        entry-clause
                        terminals-clause
                        (reverse nonterminal-clauses))
                (let ([cl (car cl*)] [cl* (cdr cl*)])
                  (syntax-case cl (extends entry terminals)
                    [(extends . _)
                     (begin
                       (when extension-clause
                         (syntax-violation who "duplicate extends clause" x cl))
                       (f cl* cl entry-clause terminals-clause nonterminal-clauses))]
                    [(entry . _)
                     (begin
                       (when entry-clause
                         (syntax-violation who "duplicate entry clause" x cl))
                       (f cl* extension-clause cl terminals-clause nonterminal-clauses))]
                    [(terminals . _)
                     (begin
                       (when terminals-clause
                         (syntax-violation who "duplicate nonterminals clause" x cl))
                       (f cl* extension-clause entry-clause cl nonterminal-clauses))]
                    [(non-terminal-name . _)
                     (identifier? #'non-terminal-name)
                     (f cl* extension-clause entry-clause terminals-clause
                        (cons cl nonterminal-clauses))]
                    [_ (syntax-violation who "invalid clause" x cl)]))))))
      (define parse-extension-clause
        (lambda (cl)
          (and cl
               (syntax-case cl ()
                 [(_ language-name) (identifier? #'language-name) #'language-name]
                 [_ (syntax-violation who "invalid extends clause" x cl)]))))
      (syntax-case x ()
        [(_ language-name clause ...)
         (identifier? #'language-name)
         (let-values ([(extension-clause entry-clause terminals-clause nonterminal-clauses)
                       (parse-clauses #'(clause ...))])
           (define base-language-name (parse-extension-clause extension-clause))
           (define extended? (and base-language-name #t))
           (with-syntax ([stx x]
                         [base-language-name base-language-name]
                         [entry-clause entry-clause]
                         [terminals-clause terminals-clause]
                         [nonterminal-clauses nonterminal-clauses])
             (if extended?
                 #'(base-language-name extend-language-key stx language-name entry-clause terminals-clause nonterminal-clauses)
                 #'(define-extended-language stx language-name entry-clause terminals-clause nonterminal-clauses))))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax define-extended-language
    (lambda (x)
      (define who 'define-language)
      (define generate-language-definition
        (lambda (stx language-name entry-clause terminals nonterminals)
          (define parse-entry-clause
            (lambda (cl)
              (syntax-case cl ()
                [(_ nonterminal-name)
                 (identifier? #'nonterminal-name)
                 #'nonterminal-name]
                [#f
                 (syntax-case nonterminals ()
                   [((nonterminal-name . _) . _)
                    #'nonterminal-name])]
                [_ (syntax-violation who "invalid entry clause" stx cl)])))
          (when (null? nonterminals)
            (syntax-violation who "missing nonterminal clause" stx))
          (let ([entry (parse-entry-clause entry-clause)])
            (with-syntax ([language-name language-name]
                          [entry entry]
                          [terminals terminals]
                          [nonterminals nonterminals])
              #'(define-syntax language-name
                  (make-language-transformer #'language-name #'entry #'terminals #'nonterminals))))))
      (syntax-case x ()
        [(_ stx language-name (base-terminal ...) (base-nonterminal ...)
            entry-clause terminals-clause (nonterminal-clause ...))
         (let ([stx #'stx])
           (define terminals (parse-extended-terminals-clause stx #'(base-terminal ...) #'terminals-clause))
           (define nonterminals (filter (lambda (cl) (not (null? cl)))
                                        (parse-extended-nonterminal-clauses stx
                                                                            #'(base-nonterminal ...)
                                                                            #'(nonterminal-clause ...))))
           (generate-language-definition stx #'language-name #'entry-clause terminals nonterminals))]
        [(_ stx language-name
            entry-clause terminals-clause (nonterminal-clause ...))
         (let ([stx #'stx])
           (define terminals (parse-terminals-clause stx #'terminals-clause))
           (define nonterminals (parse-nonterminal-clauses stx #'(nonterminal-clause ...))


             (map (lambda (cl)
                    (parse-nonterminal-clause stx cl))
                  #'(nonterminal-clause ...)))
           (generate-language-definition stx #'language-name #'entry-clause terminals nonterminals))])))

  (define make-language-transformer
    (lambda (who entry terminals nonterminals)
      (lambda (x)
        (syntax-case x (extend-language-key language->datum-key)
          [(_ extend-language-key stx language-name entry-clause terminals-clause nonterminal-clauses)
           (with-syntax ([terminals terminals]
                         [nonterminals nonterminals])
             #'(define-extended-language stx language-name terminals nonterminals
                 entry-clause terminals-clause nonterminal-clauses))]
          [(_ language->datum-key)
           (with-syntax ([who-name who]
                         [entry-name entry]
                         [(terminal-clause ...) terminals]
                         [(nonterminal-clause ...) nonterminals])
             #''(define-language who-name
                  (entry entry-name)
                  (terminals terminal-clause ...)
                  nonterminal-clause ...))]
          [_ (syntax-violation who "invalid use of language name" x)]))))

  (define-syntax/who language->datum
    (lambda (x)
      (syntax-case x ()
        [(_ language-name)
         (identifier? #'language-name)
         #'(language-name language->datum-key)]
        [_ (syntax-violation who "invalid syntax" x)]))))
