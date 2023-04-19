#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $parsers)
  (export
    parse-import-form
    parse-library-name-part
    parse-sub-version)
  (import
    (rnrs)
    (scheme-libraries numbers)
    (scheme-libraries syntax exceptions)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects))

  (define parse-import-form
    (lambda (x)
      (syntax-match x
	[(import ,imp* ...) imp*]
	[,_
	 (syntax-error #f "invalid import form" x)])))

  (define parse-library-name-part
    (lambda (x)
      (unless ($identifier? x)
        (syntax-error #f "invalid library name part" #f x))
      (identifier->symbol x)))

  (define parse-sub-version
    (lambda (x)
      (let ([e (syntax-object->datum x)])
        (unless (exact-nonnegative-integer? e)
          (syntax-error #f "invalid library sub-version" #f x))
        e))))
