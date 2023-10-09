#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries reading tokenizers)
  (export
    make-tokenizer
    tokenizer?
    tokenizer-get-lexeme
    tokenizer-lexical-error
    &lexical-error
    make-lexical-error
    lexical-error?)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries format-conditions)
    (scheme-libraries filenames)
    (scheme-libraries ports)
    (scheme-libraries reading lexemes)
    (scheme-libraries reading positions)
    (scheme-libraries reading source-locations)
    (scheme-libraries unicode))

  (define-record-type tokenizer
    (nongenerative tokenizer-9bfff74b-f81b-42d5-bd2b-63896dd95433)
    (fields
      port
      filename
      (mutable position))
    (protocol
      (lambda (new)
        (define who 'make-tokenizer)
        (lambda (port filename)
          (unless (textual-input-port? port)
            (assertion-violation who "invalid port argument" port))
          (unless (or (not filename) (filename? filename))
            (assertion-violation who "invalid filename argument" filename))
          (new port filename (make-position))))))

  (define/who tokenizer-lexical-error
    (lambda (tokenizer msg start end . irr*)
      (define filename
        (begin
          (unless (tokenizer? tokenizer)
            (assertion-violation who "invalid tokenizer argument" tokenizer))
          (tokenizer-filename tokenizer)))
      (unless (string? msg)
        (assertion-violation who "invalid message argument" msg))
      (unless (position? start)
        (assertion-violation who "invalid start argument" start))
      (unless (position? end)
        (assertion-violation who "invalid end argument" end))
      (let*
          ([err (condition (make-lexical-error)
                           (make-message-condition msg)
                           (make-format-condition))]
           [err (if (null? irr*) err
                    (condition err
                               (make-irritants-condition irr*)))]
           [err (if (not filename) err
                    (condition err
                               (make-source-location-condition
                                (make-source-location
                                 filename
                                 start
                                 end))))])
        (raise err))))

  (define/who tokenizer-get-lexeme
    (lambda (tokenizer read)
      (unless (tokenizer? tokenizer)
        (assertion-violation who "invalid tokenizer argument" tokenizer))
      (unless (procedure? read)
        (assertion-violation who "invalid read argument" read))
      (let ([port (tokenizer-port tokenizer)]
            [filename (tokenizer-filename tokenizer)])
        (define read-char
          (lambda ()
            (let ([ch (get-char port)])
              (unless (eof-object? ch)
                (case ch
                  [(#\newline)
                   (position-set! (position-lines (position) 1))]
                  [(#\tab)
                   (position-set! (position-tabulator (position)))]
                  [else
                   (position-set! (position-columns (position) (unicode-width ch)))]))
              ch)))
        (define peek-char
          (lambda ()
            (lookahead-char port)))
        (define read-delimited-lexeme
          (lambda (initial)
            (let f ([ch* (reverse (string->list initial))])
              (if (delimiter? (peek-char))
                  (list->string (reverse ch*))
                  (f (cons (read-char) ch*))))))
        (define read-number-prefix
          (lambda (initial)
            (cond
             [(char=? (peek-char) #\#)
              (read-char)
              (string-append initial (string #\# (read-char)))]
             [else initial])))
        (define read-inline-hex-escape
          (lambda ()
            (let ([start (position)])
              (let f ([val 0] [empty? #t])
                (let ([ch (read-char)])
                  (cond
                   [(eof-object? ch)
                    (lexical-error "unterminated inline hex escape" start (position))]
                   [(char=? ch #\;)
                    (when empty?
                      (lexical-error "empty inline hex escape" start (position)))
                    (unless (unicode-scalar-value? val)
                      (lexical-error "invalid Unicode scalar value ~s" start (position)))
                    (integer->char val)]
                   [(hex-digit ch)
                    =>
                    (lambda (e)
                      (f (fx+ (fx* val 16) e) #f))]
                   [else
                    (lexical-error "invalid inline hex escape syntax" start (position))]))))))
        (define position
          (lambda ()
            (tokenizer-position tokenizer)))
        (define position-set!
          (lambda (position)
            (tokenizer-position-set! tokenizer position)))
        (define lexical-error
          (lambda (msg start end . irr*)
            (apply tokenizer-lexical-error
                   tokenizer msg start end irr*)))
        (define skip-intraline-whitespace
          (lambda ()
            (do ()
                ((not (intraline-whitespace? (peek-char))))
              (read-char))))
        (let f ()
          (let* ([start (position)]
                 [ch (read-char)])
            (define skip-line-comment
              (lambda ()
                (let ([ch (read-char)])
                  (cond
                   [(eof-object? ch)
                    (lexical-error "unterminated line comment" start (position))]
                   [(char=? ch #\return)
                    (when (memv (peek-char) '(#\linefeed #\x0085))
                      (read-char))]
                   [(and (not (line-ending? ch))
                         (not (char=? ch #\x2029)))
                    (skip-line-comment)]))))
            (define skip-nested-comment
              (lambda ()
                (let f ([depth 0])
                  (let ([ch (read-char)])
                    (cond
                     [(eof-object? ch)
                      (lexical-error "unterminated nested comment" start (position))]
                     [(and (char=? ch #\#)
                           (char=? (peek-char) #\|))
                      (read-char)
                      (f (fx+ depth 1))]
                     [(and (char=? ch #\|)
                           (char=? (peek-char) #\#))
                      (read-char)
                      (when (fxpositive? depth)
                        (f (fx- depth 1)))]
                     [else
                      (f depth)])))))
            (define get-boolean
              (lambda (val)
                (unless (delimiter? (peek-char))
                  (lexical-error "unterminated boolean" start (position)))
                (make-atomic val start (position))))
            (define get-number
              (lambda (initial)
                (let* ([initial (if (char=? (string-ref initial 0) #\#)
                                    (read-number-prefix initial)
                                    initial)]
                       [s (read-delimited-lexeme initial)])
                  (cond
                   [(string->number s)
                    =>
                    (lambda (num)
                      (make-atomic num start (position)))]
                   [else
                    (lexical-error "invalid number syntax ~s" start (position) s)]))))
            (define get-character
              (lambda ()
                (define c
                  (let ([ch (read-char)])
                    (cond
                     [(eof-object? ch) (lexical-error "unterminated character" start (position))]
                     [(char=? ch #\x)
                      (let ([ch (read-inline-hex-escape)])
                        (unless (delimiter? ch)
                          (lexical-error "undelimited character" start (position)))
                        ch)]
                     [else
                      (let ([s (read-delimited-lexeme (string ch))])
                        (if (fx=? (string-length s) 1)
                            (string-ref s 0)
                            (cond
                             [(assq (string->symbol s) '((nul . #\nul)
                                                         (alarm . #\alarm)
                                                         (backspace . #\backspace)
                                                         (tab . #\tab)
                                                         (linefeed . #\linefeed)
                                                         (newline . #\newline)
                                                         (vtab . #\vtab)
                                                         (page . #\page)
                                                         (return . #\return)
                                                         (esc . #\esc)
                                                         (space . #\space)
                                                         (delete . #\delete)))
                              => cdr]
                             [else
                              (lexical-error "invalid character name" start (position))])))])))
                (make-atomic c start (position))))
            (define get-identifier
              (lambda (initial)
                (let ([id (read-identifier initial)])
                  (make-atomic id start (position)))))
            (define get-string
              (lambda ()
                (let f ([ch* '()])
                  (let* ([middle (position)]
                         [ch (read-char)])
                    (cond
                     [(eof-object? ch)
                      (lexical-error "unterminated string" start (position))]
                     [(char=? ch #\")
                      (make-atomic (list->string (reverse ch*)) start (position))]
                     [(char=? ch #\\)
                      (let ([ch (read-char)])
                        (cond
                         [(eof-object? ch)
                          (lexical-error "unterminated string" start (position))]
                         [(assv ch '((#\a . #\alarm)
                                     (#\b . #\backspace)
                                     (#\t . #\tab)
                                     (#\n . #\linefeed)
                                     (#\v . #\vtab)
                                     (#\f . #\page)
                                     (#\r . #\return)
                                     (#\" . #\")
                                     (#\\ . #\\)))
                          =>
                          (lambda (e)
                            (f (cons (cdr e) ch*)))]
                         [(char=? ch #\x)
                          (f (cons (read-inline-hex-escape) ch*))]
                         [else
                          (skip-intraline-whitespace)
                          (let ([ch (read-char)])
                            (cond
                             [(char=? ch #\return)
                              (when (memv (peek-char) '(#\linefeed #\x0085))
                                (read-char))]
                             [(and (not (line-ending? ch)))
                              (lexical-error "invalid string element" middle (position))]))
                          (skip-intraline-whitespace)
                          (f ch*)]))]
                     [(char=? ch #\return)
                      (when (memv (peek-char) '(#\linefeed #\x0085))
                        (read-char))
                      (f (cons #\newline ch*))]
                     [(line-ending? ch)
                      (f (cons #\newline ch*))]
                     [else
                      (f (cons ch ch*))])))))
            (define read-identifier
              (lambda (initial)
                (let ([s (read-delimited-lexeme initial)])
                  (or (string->identifier s)
                      (lexical-error "invalid identifier syntax ~s" start (position) s)))))
            (define handle-flag
              (lambda (flag)
                (case flag
                  [(r6rs) (values)]
                  [else
                   (lexical-error "unknown reader flag #!~s" start (position) flag)])))
            (cond
             ;; #!eof
             [(eof-object? ch) (make-end-of-input start (position))]
             ;; <whitespace>
             [(whitespace? ch) (f)]
             ;; <initial>
             [(initial? ch) (get-identifier (string ch))]
             ;; <digit>
             [(digit? ch) (get-number (string ch))]
             ;; ;
             [(char=? ch #\;) (skip-line-comment) (f)]
             ;; (
             [(char=? ch #\() (make-left-parenthesis start (position))]
             ;; )
             [(char=? ch #\)) (make-right-parenthesis start (position))]
             ;; [
             [(char=? ch #\[) (make-left-bracket start (position))]
             ;; ]
             [(char=? ch #\]) (make-right-bracket start (position))]
             ;; +
             [(char=? ch #\+)
              (if (delimiter? (peek-char))
                  (make-atomic '+ start (position))
                  (get-number "+"))]
             ;; -
             [(char=? ch #\-)
              (let ([ch (peek-char)])
                (cond
                 [(delimiter? ch)
                  (make-atomic '- start (position))]
                 [(char=? ch #\>)
                  (read-char)
                  (get-identifier "->")]
                 [else (get-number "-")]))]
             ;; '
             [(char=? ch #\') (make-abbreviation 'quote start (position))]
             ;; `
             [(char=? ch #\`) (make-abbreviation 'quasiquote start (position))]
             ;; ,
             [(char=? ch #\,)
              (cond
               [(char=? (peek-char) #\@)
                (read-char)
                (make-abbreviation 'unquote-splicing start (position))]
               [else (make-abbreviation 'unquote start (position))])]
             ;; .
             [(char=? ch #\.)
              (let ([ch (peek-char)])
                (cond
                 [(delimiter? (peek-char))
                  (make-dot start (position))]
                 [(char=? ch #\.)
                  (read-char)
                  (unless (and (char=? (read-char) #\.)
                               (delimiter? (peek-char)))
                    (lexical-error "invalid peculiar identifier" start (position)))
                  (make-atomic '... start (position))]
                 [else (get-number ".")]))]
             ;; #
             [(char=? ch #\#)
              (let ([ch (read-char)])
                (cond
                 ;; #
                 [(eof-object? ch)
                  (lexical-error "unterminated sharp syntax" start (position))]
                 ;; #!
                 [(char=? ch #\!)
                  (handle-flag (read-identifier ""))
                  (f)]
                 ;; #(
                 [(char=? ch #\()
                  (make-vector-prefix start (position))]
                 ;; #vu8
                 [(char=? ch #\v)
                  (cond
                   [(and (char=? (read-char) #\u)
                         (char=? (read-char) #\8)
                         (char=? (read-char) #\())
                    (make-bytevector-prefix start (position))]
                   [else (lexical-error "invalid homogeneous vector syntax" start (position))])]
                 ;; #'
                 [(char=? ch #\') (make-abbreviation 'syntax start (position))]
                 ;; #`
                 [(char=? ch #\`) (make-abbreviation 'quasisyntax start (position))]
                 ;; ,
                 [(char=? ch #\,)
                  (cond
                   [(char=? (peek-char) #\@)
                    (read-char)
                    (make-abbreviation 'unsyntax-splicing start (position))]
                   [else (make-abbreviation 'unsyntax start (position))])]
                 ;; #;
                 [(char=? ch #\;)
                  (when (eof-object? (read))
                    (lexical-error "unterminated datum comment" start (position)))
                  (f)]
                 ;; #|
                 [(char=? ch #\|) (skip-nested-comment) (f)]
                 ;; t | T
                 [(or (char=? ch #\t)
                      (char=? ch #\T))
                  (get-boolean #t)]
                 ;; f | F
                 [(or (char=? ch #\f)
                      (char=? ch #\F))
                  (get-boolean #f)]
                 ;; i | I | e | E | b | B | o | O | d | D | x | X
                 [(memv ch '(#\i #\I #\e #\E #\b #\B #\o #\O #\d #\D #\x #\X))
                  (get-number (string #\# ch))]
                 ;; \
                 [(char=? ch #\\) (get-character)]
                 [else
                  (lexical-error "invalid sharp syntax ~a" start (position) ch)]))]
             ;; "
             [(char=? ch #\") (get-string)]
             ;; \
             [(char=? ch #\\) (get-identifier "\\")]
             ;; <invalid>
             [else
              (lexical-error "invalid character ~a" start (position) ch)]))))))

  (define delimiter?
    (lambda (ch)
      (or (eof-object? ch)
	  (memv ch '(#\( #\) #\" #\; #\[ #\]))
	  (whitespace? ch))))

  (define whitespace?
    (lambda (ch)
      (or (memv ch '(#\tab #\linefeed #\vtab #\page #\return #\newline))
	  (memv (char-general-category ch) '(Zs Zl Zp)))))

  (define intraline-whitespace?
    (lambda (ch)
      (or (char=? ch #\tab)
	  (symbol=? (char-general-category ch) 'Zs))))

  (define line-ending?
    (lambda (ch)
      (memv ch '(#\linefeed #\return #\x0085 #\x2028))))

  (define initial?
    (lambda (c)
      (or (constituent? c)
	  (special-initial? c))))

  (define constituent?
    (lambda (c)
      (or (letter? c)
	  (and (not (ascii? c))
	       (memv (char-general-category c)
		     '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))))))

  (define letter?
    (lambda (c)
      (or (char<=? #\a c #\z)
	  (char<=? #\A c #\Z))))

  (define special-initial?
    (lambda (c)
      (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))))

  (define digit?
    (lambda (c)
      (char<=? #\0 c #\9)))

  (define subsequent?
    (lambda (c)
      (or (initial? c)
	  (digit? c)
	  (special-subsequent? c)
	  (memv (char-general-category c)
		'(Nd Mc Me)))))

  (define special-subsequent?
    (lambda (c)
      (memv c '(#\+ #\- #\. #\@))))

  (define ascii?
    (lambda (c)
      (fx<=? (char->integer c) 127)))

  (define hex-digit
    (lambda (ch)
      (let ([e (char->integer ch)])
        (cond
         [(fx<=? #x30 e #x39) (fx- e #x30)]
         [(fx<=? #x41 e #x46) (fx- e #x37)]
         [(fx<=? #x61 e #x66) (fx- e #x57)]
         [else #f]))))

  (define string->identifier
    (lambda (s)
      (define n (string-length s))
      (cond
       [(string=? s "+") '+]
       [(string=? s "-") '-]
       [(string=? s "...") '...]
       [(and (>= n 2)
             (string=? (substring s 0 2) "->"))
        (subsequent*->identifier s 2 "->")]
       [(and (>= n 1)
             (initial? (string-ref s 0)))
        (subsequent*->identifier s 1 (substring s 0 1))]
       [(and (>= n 1)
             (char=? (string-ref s 0) "\\"))
        (inline-hex-escape s 1
                           (lambda (ch k)
                             (subsequent*->identifier s k (string ch))))]
       [else
        #f])))

  (define subsequent*->identifier
    (lambda (s k prefix)
      (define n (string-length s))
      (let f ([k k] [ch* (reverse (string->list prefix))])
        (if (= k n)
            (string->symbol (list->string (reverse ch*)))
            (let ([ch (string-ref s k)])
              (cond
               [(subsequent? ch) (f (fx+ k 1) (cons ch ch*))]
               [(char=? ch #\\)
                (inline-hex-escape s (fx+ k 1)
                                   (lambda (ch k)
                                     (f k (cons ch ch*))))]
               [else #f]))))))

  (define inline-hex-escape
    (lambda (s i k)
      (define n (string-length s))
      (and (> n i)
           (char=? (string-ref s i) #\x)
           (let f ([i (fx+ i 1)] [val 0] [empty? #t])
             (and (> n i)
                  (let ([ch (string-ref s i)])
                    (cond
                     [(char=? ch #\;)
                      (and (not empty?)
                           (unicode-scalar-value? val)
                           (k (integer->char val) (fx+ i 1)))]
                     [(hex-digit ch)
                      =>
                      (lambda (e)
                        (f (fx+ i 1) (fx+ (fx* val 16) e) #f))])))))))

  (define-condition-type &lexical-error &error
    make-lexical-error lexical-error?))
