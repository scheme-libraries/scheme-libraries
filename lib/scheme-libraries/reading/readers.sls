#!r6rs

(library (scheme-libraries reading readers)
  (export
    make-reader
    reader?
    reader-get-annotated-datum)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries filenames)
    (scheme-libraries ports)
    (scheme-libraries reading annotated-datums)
    (scheme-libraries reading lexemes)
    (scheme-libraries reading source-locations)
    (scheme-libraries reading tokenizers))

  (define-record-type reader
    (nongenerative reader-8a5dd7d3-e0ca-4040-bb8a-01613a3de1f9)
    (fields
      tokenizer
      filename)
    (protocol
      (lambda (new)
        (define who 'make-reader)
        (lambda (port filename)
          (unless (textual-input-port? port)
            (assertion-violation who "invalid port argument" port))
          (unless (or (not filename) (filename? filename))
            (assertion-violation who "invalid filename argument" filename))
          (new (make-tokenizer port filename) filename)))))

  (define/who reader-get-annotated-datum
    (lambda (reader)
      (unless (reader? reader)
        (assertion-violation who "invalid reader argument" reader))
      (let ([tokenizer (reader-tokenizer reader)]
            [filename (reader-filename reader)])
        (define lexical-error
          (lambda (msg start end . irr*)
            (apply tokenizer-lexical-error
                   tokenizer msg start end irr*)))
        (let read ()
          (define get-lexeme
            (lambda ()
              (tokenizer-get-lexeme tokenizer read)))
          (let ([lex (get-lexeme)])
            (let f ([lex lex])
              (let ([start (lexeme-start lex)]
                    [end (lexeme-end lex)])
                (define source-location
                  (lambda ()
                    (make-source-location filename start end)))
                (define get-compound
                  (lambda (bracketed? type)
                    (let g ([e* '()])
                      (let ([lex (get-lexeme)])
                        (define source-location
                          (lambda ()
                            (make-source-location filename start (lexeme-end lex))))
                        (define finish-compound
                          (lambda ()
                            (case type
                              [(list)
                               (make-annotated-list (reverse e*) (source-location))]
                              [(vector)
                               (make-annotated-vector (reverse e*) (source-location))]
                              [(bytevector)
                               (make-annotated-atom
                                (u8-list->bytevector
                                 (map annotated-datum-value (reverse e*)))
                                (source-location))]
                              [else (assert #f)])))
                        (cond
                         [(end-of-input? lex)
                          (lexical-error "unterminated datum" start (lexeme-end lex))]
                         [(right-parenthesis? lex)
                          (when bracketed?
                            (lexical-error "bracketed list terminated by parenthesis" start (lexeme-end lex)))
                          (finish-compound)]
                         [(right-bracket? lex)
                          (unless bracketed?
                            (lexical-error "parenthesized datum terminated by bracket" start (lexeme-end lex)))
                          (finish-compound)]
                         [(dot? lex)
                          (unless (symbol=? type (compound-type list))
                            (lexical-error "unexpected dot in compound datum" start (lexeme-end lex)))
                          (let ([lex (get-lexeme)])
                            (when (end-of-input? lex)
                              (lexical-error "unterminated dotted list" start (lexeme-end lex)))
                            (let* ([e (f lex)]
                                   [lex (get-lexeme)])
                              (define finish-dotted-list
                                (lambda ()
                                  (make-annotated-dotted-list (reverse e*)
                                                              e
                                                              (make-source-location filename
                                                                                    start
                                                                                    (lexeme-end lex)))))
                              (cond
                               [(end-of-input? lex)
                                (lexical-error "unterminated dotted list" start (lexeme-end lex))]
                               [(right-parenthesis? lex)
                                (when bracketed?
                                  (lexical-error "bracketed dotted list terminated by parenthesis" start (lexeme-end lex)))
                                (finish-dotted-list)]
                               [(right-bracket? lex)
                                (unless bracketed?
                                  (lexical-error "parenthesized dotted list terminated by bracket" start (lexeme-end lex)))
                                (finish-dotted-list)]
                               [else
                                (lexical-error "invalid dotted list syntax" start (lexeme-end lex))])))]
                         [else
                          (let ([e (f lex)])
                            (let ([val (annotated-datum-value e)])
                              (when (and (symbol=? type (compound-type bytevector))
                                         (not (and (fixnum? val)
                                                   (fx<=? 0 val 255))))
                                (lexical-error "invalid octet ~a" (lexeme-start lex) (lexeme-end lex) val)))
                            (g (cons e e*)))])))))
                (cond
                 [(end-of-input? lex) (eof-object)]
                 [(atomic? lex)
                  (make-annotated-atom (atomic-value lex) (source-location))]
                 [(left-parenthesis? lex)
                  (get-compound #f (compound-type list))]
                 [(left-bracket? lex)
                  (get-compound #t (compound-type list))]
                 [(right-parenthesis? lex)
                  (lexical-error "unexpected right parenthesis" start end)]
                 [(right-bracket? lex)
                  (lexical-error "unexpected right bracket" start end)]
                 [(vector-prefix? lex)
                  (get-compound #f (compound-type vector))]
                 [(bytevector-prefix? lex)
                  (get-compound #f (compound-type bytevector))]
                 [(abbreviation? lex)
                  (let ([e (read)])
                    (make-annotated-list (list (make-annotated-atom (abbreviation-symbol lex) (source-location)) e)
                                         (make-source-location filename
                                                               start
                                                               (source-location-end (annotated-datum-source-location e)))))]
                 [(dot? lex)
                  (lexical-error "unexpected dot" start end)]
                 [else (assert #f)]))))))))


  (define-enumeration compound-type
    (list vector bytevector)
    compound-types))
