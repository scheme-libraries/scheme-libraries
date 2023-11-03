#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax exceptions)
  (export
    syntax-error
    undefined-error
    identifier-error
    current-who
    current-form
    default-exception-handler
    display-condition)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (rename (scheme-libraries display-condition)
      (display-condition $display-condition))
    (scheme-libraries reading source-locations)
    (scheme-libraries syntax syntax-objects)
    (scheme-libraries thread-parameters))

  (define current-who (make-thread-parameter #f))

  (define current-form (make-thread-parameter #f))

  (define syntax-error
    (case-lambda
      [(who msg)
       (syntax-error msg #f #f)]
      [(who msg form)
       (syntax-error who msg form #f)]
      [(who msg form subform)
       (let ([who (or who (current-who))]
             [form (or form (current-form))])
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
	   (raise err)))]))

  (define/who identifier-error
    (case-lambda
      [(who id msg)
       (identifier-error who id msg id)]
      [(who id msg form)
       (unless ($identifier? id)
         (assertion-violation who "not an identifier" id))
       (syntax-error who
		     (format msg (identifier->symbol id))
		     form
		     (and (not (eqv? form id)) id))]))

  (define/who undefined-error
    (lambda (id msg)
      (unless ($identifier? id)
        (assertion-violation who "invalid identifier argument" id))
      (guard (c [(syntax-error? c)
                 (raise-continuable
                  (condition c (make-undefined-error)))])
        (syntax-error #f (format msg (identifier->symbol id)) id))))

  (define default-exception-handler
    (lambda (obj)
      (display-condition obj)
      (newline (current-error-port))
      (unless (and (condition? obj)
                   (not (serious-condition? obj)))
        (exit #f))))

  (define display-condition
    (lambda (obj)
      ($display-condition (if (and (source-location-condition? obj)
                                   (message-condition? obj))
                              (condition
                                (make-message-condition (format "~s: ~s"
                                                          (format-source-location (condition-source-location obj))
                                                          (condition-message obj)))
                                obj)
                              obj))))
  )
