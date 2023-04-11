#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax exceptions)
  (export
    syntax-error)
  (import
    (rnrs)
    (scheme-libraries reading source-locations)
    (scheme-libraries syntax syntax-objects))

  (define syntax-error
    (case-lambda
      [(who msg)
       (syntax-error who msg #f #f)]
      [(who msg form)
       (syntax-error who msg form #f)]
      [(who msg form subform)
       (let* ([err (condition (make-syntax-error form subform)
			      (make-message-condition msg))]
              [err (cond
                    [(and (syntax-object? form)
                          (syntax-object-source-location form))
                     => (lambda (source-location)
                          (condition (make-source-location-condition
                                      source-location)
                                     err))]
                    [else err])]
	      [err (if who (condition (make-who-condition who) err) err)])
	 (raise err))]))


)
