#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries reading lexemes)
  (export
    lexeme?
    lexeme-start
    lexeme-end
    make-end-of-input
    end-of-input?
    make-atomic
    atomic?
    atomic-value
    make-left-parenthesis
    left-parenthesis?
    make-right-parenthesis
    right-parenthesis?
    make-left-bracket
    left-bracket?
    make-right-bracket
    right-bracket?
    make-vector-prefix
    vector-prefix?
    make-bytevector-prefix
    bytevector-prefix?
    make-abbreviation
    abbreviation?
    abbreviation-symbol
    make-dot
    dot?)
  (import
    (rnrs)
    (scheme-libraries atom)
    (scheme-libraries reading positions))

  (define-record-type lexeme
    (nongenerative lexeme-e3f68c98-541a-4a3b-8d9b-6de5897bc822)
    (fields
      start
      end)
    (protocol
      (lambda (new)
        (lambda (who start end)
          (unless (position? start)
            (assertion-violation who "invalid start argument" start))
          (unless (position? end)
            (assertion-violation who "invalid end argument" end))
          (new start end)))))

  (define-record-type end-of-input
    (nongenerative end-of-input-c77f6a89-f73c-4807-9a63-bc70540dee0e)
    (parent lexeme) (sealed #t)
    (protocol
      (lambda (pargs->new)
        (define who 'make-end-of-input)
        (lambda (start end)
          ((pargs->new who start end))))))


  (define-record-type atomic
    (nongenerative atomic-6e0906d9-5da3-40d4-a459-e54b245db5b7)
    (parent lexeme) (sealed #t)
    (fields value)
    (protocol
      (lambda (pargs->new)
        (define who 'make-atomic)
        (lambda (value start end)
          (unless (atom? value)
            (assertion-violation who "invalid value argument" value))
          ((pargs->new who start end) value)))))

  (define-record-type left-parenthesis
    (nongenerative left-parenthesis-38278d51-b043-4bcd-8fc7-8f920bc8f6d9)
    (parent lexeme) (sealed #t)
    (protocol
      (lambda (pargs->new)
        (define who 'make-left-parenthesis)
        (lambda (start end)
          ((pargs->new who start end))))))

  (define-record-type right-parenthesis
    (nongenerative right-parenthesis-3e873089-b0d4-434a-a020-f35037ab8c7d)
    (parent lexeme) (sealed #t)
    (protocol
      (lambda (pargs->new)
        (define who 'make-right-parenthesis)
        (lambda (start end)
          ((pargs->new who start end))))))

  (define-record-type left-bracket
    (nongenerative left-parenthesis-38278d51-b043-4bcd-8fc7-8f920bc8f6d9)
    (parent lexeme) (sealed #t)
    (protocol
      (lambda (pargs->new)
        (define who 'make-left-parenthesis)
        (lambda (start end)
          ((pargs->new who start end))))))

  (define-record-type right-bracket
    (nongenerative right-parenthesis-3e873089-b0d4-434a-a020-f35037ab8c7d)
    (parent lexeme) (sealed #t)
    (protocol
      (lambda (pargs->new)
        (define who 'make-right-parenthesis)
        (lambda (start end)
          ((pargs->new who start end))))))

  (define-record-type vector-prefix
    (nongenerative vector-prefix-d0d80e19-b383-4eed-9227-c48ffe322889)
    (parent lexeme) (sealed #t)
    (protocol
      (lambda (pargs->new)
        (define who 'make-vector-prefix)
        (lambda (start end)
          ((pargs->new who start end))))))

  (define-record-type bytevector-prefix
    (nongenerative bytevector-prefix-fc047344-2ac1-49e0-a6c9-503e1cdc68a7)
    (parent lexeme) (sealed #t)
    (protocol
      (lambda (pargs->new)
        (define who 'make-bytevector-prefix)
        (lambda (start end)
          ((pargs->new who start end))))))

  (define-record-type abbreviation
    (nongenerative abbreviation-e8f11087-d26c-4eb1-b2e5-982d1ba2946a)
    (parent lexeme) (sealed #t)
    (fields symbol)
    (protocol
      (lambda (pargs->new)
        (define who 'make-abbreviation)
        (lambda (symbol start end)
          (unless (symbol? symbol)
            (assertion-violation who "invalid symbol argument" symbol))
          ((pargs->new start end) symbol)))))

  (define-record-type dot
    (nongenerative dot-2c1a4b1f-190a-475a-a52b-7ff1c2b16945)
    (parent lexeme) (sealed #t)
    (protocol
      (lambda (pargs->new)
        (define who 'make-dot)
        (lambda (start end)
          ((pargs->new who start end)))))))
