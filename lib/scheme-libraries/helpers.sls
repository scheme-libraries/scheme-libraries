#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries helpers)
  (export
    construct-name
    ellipsis?
    symbolic-identifier=?
    syntax-quote
    constant?)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define/who symbolic-identifier=?
    (lambda (id1 id2)
      (unless (identifier? id1)
        (assertion-violation who "invalid first identifier argument" id1))
      (unless (identifier? id2)
        (assertion-violation who "invalid second identifier argument" id2))
      (symbol=? (syntax->datum id1)
                (syntax->datum id2))))

  (define/who construct-name
    (lambda (k . arg*)
      (unless (identifier? k)
        (assertion-violation who "invalid template identifier argument" k))
      (datum->syntax
       k
       (string->symbol
	(apply string-append
	       (map (lambda (x)
		      (cond
                       [(string? x) x]
                       [(identifier? x)
			(symbol->string (syntax->datum x))]
                       [else
                        (assertion-violation who "invalid argument" x)]))
		    arg*))))))

  (define ellipsis?
    (lambda (x)
      (and (identifier? x)
	   (free-identifier=? x #'(... ...)))))

  (define constant?
    (lambda (x)
      (or (boolean? x)
          (bytevector? x)
          (char? x)
          (number? x)
          (string? x))))

  (define syntax-quote
    (lambda (x)
      #`'#,(datum->syntax #'* x)))

  )
