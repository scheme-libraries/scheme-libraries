#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $helpers)
  (export
    $construct-name)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries syntax syntax-objects))

  (define/who $construct-name
    (lambda (k . arg*)
      (unless ($identifier? k)
        (assertion-violation who "invalid template identifier argument" k))
      (datum->syntax-object
       k
       (string->symbol
	(apply string-append
	       (map (lambda (x)
		      (cond
                       [(string? x) x]
                       [($identifier? x)
			(symbol->string (syntax-object->datum x))]
                       [else
                        (assertion-violation who "invalid argument" x)]))
		    arg*))))))
)
