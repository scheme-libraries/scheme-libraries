#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries debug)
  (export
    debug
    trace)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries info))

  (define-syntax/who debug
    (lambda (stx)
      (syntax-case stx ()
        [(debug . form)
         ;; TODO: Implement and check debug level.
         #'form]
        [_ (syntax-violation who "invalid syntax" stx)])))

  (define-syntax/who trace
    (lambda (stx)
      (syntax-case stx (define lambda)
        [(_ . (define id (lambda arg* body1 ... body2)))
         #'(define id
             (lambda arg*
               (dynamic-wind
                 (lambda ()
                   (info "Entering ~s" 'id))
                 (lambda ()
                   body1 ... body2)
                 (lambda ()
                   (info "Leaving ~s" 'id)))))]
        [_ (syntax-violation who "invalid syntax" stx)])))
  )
