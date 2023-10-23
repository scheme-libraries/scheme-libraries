#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries reading annotated-datums)
  (scheme-libraries reading readers)
  (scheme-libraries testing))

(define-syntax test-datums
  (syntax-rules ()
    [(test-datums expected-datum ... syntax-string)
     (test-datums-unquoted `(expected-datum ...) syntax-string)]))

(define test-datums-unquoted
  (lambda (expected syntax)
    (test-equal expected
      (let* ([port (open-string-input-port syntax)]
             [reader (make-reader port #f)])
        (let f ([e* '()])
          (let ([e (reader-get-annotated-datum reader)])
            (if (eof-object? e)
                (reverse e*)
                (f (cons (annotated-datum-value e) e*)))))))))

(test-begin "reading")

(test-datums "#!r6rs")

(test-datums 28 "#e28.000")
(test-datums 28 "#x1c")
(test-datums (8 13) "(8 13)")
(test-datums (8 13) "( 08 13 )")
(test-datums (8 13) "(8 . (13 . ()))")
(test-datums #x1A "#x1A")
(test-datums #x1A "#x1a")

(test-datums "\n" "\"\r\n\"")
(test-datums "" "\"\\\r\n\"")

(test-datums "; line comment\n")
(test-datums "#; <datum>")
(test-datums "#|#||#|#")

(test-datums lambda q soup list->vector + V17a <= a34kTMNs ->- the-word-recursion-has-many-meanings
  "lambda q soup list->vector + V17a <= a34kTMNs ->- the-word-recursion-has-many-meanings")

(test-datums #t #f "#t #f")

(test-datums #(0 (2 2 2 2) "Anna") "#(0 (2 2 2 2) \"Anna\")")

(test-datums #vu8(2 24 123) "#vu8(2 24 123)")

;;; Random examples

(test-datums (unsyntax foo) (unsyntax-splicing bar) "#,foo #,@bar")
(test-datums (,'unquote foo) (,'unquote-splicing bar) ",foo ,@bar")

(test-end "reading")
