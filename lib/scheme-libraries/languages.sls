#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries languages)
  (export
    define-language
    entry
    extends
    maybe
    terminals
    language->datum)
  (import
    (rnrs)
    (scheme-libraries))

  (define-auxiliary-syntax extends)
  (define-auxiliary-syntax entry)
  (define-auxiliary-syntax maybe)
  (define-auxiliary-syntax terminals)

  (define language->datum-key)
  (define extend-language-key)

  (define-syntax/who define-language
    (lambda (x)
      (define metavars (make-eq-hashtable))
      (define parse-clauses
        (lambda (cl*)
          (let f ([cl* cl*] [extension #f] [entry #f] [terminals #f] [nonterminals '()])
            (if (null? cl*)
                (values extension entry terminals nonterminals)
                (let ([cl (car cl*)] [cl* (cdr cl*)])
                  (syntax-case cl (extends entry terminals)
                    [(extends . _)
                     (begin
                       (when extension
                         (syntax-violation who "duplicate extends clause" x cl))
                       (f cl* cl entry terminals nonterminals))]
                    [(entry . _)
                     (begin
                       (when entry
                         (syntax-violation who "duplicate entry clause" x cl))
                       (f cl* extension cl terminals nonterminals))]
                    [(terminals . _)
                     (begin
                       (when terminals
                         (syntax-violation who "duplicate nonterminals clause" x cl))
                       (f cl* extension entry cl nonterminals))]
                    [(non-terminal-name . _)
                     (identifier? #'non-terminal-name)
                     (f cl* extension entry terminals (cons cl nonterminals))]
                    [_ (syntax-violation who "invalid clause" x cl)]))))))
      (define parse-extends-clause
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
           (define base-language-name (parse-extension-clause))
           (define extended? (and base-language-name #t))
           (with-syntax ([stx x]
                         [base-language-name base-language-name]
                         [entry-clause entry-clause]
                         [terminals-clause terminals-clause]
                         [nonterminal-clauses nonterminal-clauses])
             (if extended?
                 #'(base-language-name extend-language-key stx language-name entry-clause terminals-clause nonterminal-clauses)
                 #'(internal-define-language stx language-name #f #f entry-clause terminals-clause nonterminal-clauses))))]
        [_ (syntax-violation who "invalid syntax" x)])))

  ;; TODO: Define base-language
  ;; TODO: Define extended-language
  ;; Beide sollen dann den gleichen Transformer aufrufen.

  (define-syntax internal-define-language
    (lambda (x)
      (syntax-case x ()
        [(_ stx language-name base-terminals base-nonterminals entry-clause terminals-clause nonterminal-clauses)
         (let-values ([()]))

         (let ([extended? (base-terminals)]))

         #'(define-syntax language-name
             (make-language-transformer 'language-name))])))

  (define make-language-transformer
    (lambda (who)
      (lambda (x)
        (syntax-case x (extend-language-key language->datum-key)
          [(_ extend-language-key stx language-name entry-clause terminals-clause nonterminal-clauses)
           ;; FIXME: #f #f -> base terminals and nonterminals
           #'(internal-define-language stx language-name #f #f entry-clause terminals-clause nonterminal-clauses)]
          [(_ language->datum-key)
           `(define-language ...)]
          [_ (syntax-violation who "invalid use of language name" x)]))))

  (define-syntax/who language->datum
    (lambda (x)
      (syntax-case x ()
        [(_ language-name)
         (identifier? #'language-name)
         #'(language-name language->datum-key)]
        [_ (syntax-violation who "invalid syntax" x)])))



           (define extended? (and extension #t))
           (define entry (parse-entry-clause))
           (define terminals (parse-terminals-clause extended?))
           (define nonterminals
             (map (lambda (cl)
                    (parse-nonterminal-clause cl extended?))
                  nonterminal-clauses))
           #'(define-syntax language-name
               (make-language-transformer 'language-name))

  )
