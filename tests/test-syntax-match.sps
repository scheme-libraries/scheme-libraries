#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries reading annotated-datums)
  (scheme-libraries syntax syntax-objects)
  (scheme-libraries syntax syntax-match))

(define tmpl (annotated-datum->syntax-object (datum->annotated-datum 'tmpl)))

(define-syntax $syntax
  (syntax-rules ()
    [($syntax datum)
     (datum->syntax-object tmpl 'datum)]))

(test-begin "syntax-match")

(test-eqv 'a
  (syntax-match ($syntax (b c))
    [(,b ,a) 'a]))

(test-eqv 'a
  (syntax-match ($syntax (b c))
    [(,b ... ,a) 'a]))

(test-eqv 'a
  (let ([$b ($syntax b)])
    (syntax-match ($syntax (b c))
      [(b ,a) 'a])))

(test-assert
  (let ([$b ($syntax b)])
    (syntax-match ($syntax ((b c) c))
      [(b ,a) #f]
      [,x #t])))

(test-end "syntax-match")
