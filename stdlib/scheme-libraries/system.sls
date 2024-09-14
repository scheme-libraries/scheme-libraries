#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries system)
  (export
    system
    system-meta-level)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define system
    (lambda ()
      'bootstrap))

  ;; TODO: Return the correct meta level of the scheme-libraries expander.
  (define-syntax/who system-meta-level
    (lambda (stx)
      (syntax-case stx ()
        [(k) #'#f]
        [_ (syntax-violation who "invalid syntax" stx)])))
  )
