#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs conditions (6))
  (export
    ;; Conditions
    &condition
    condition
    simple-conditions
    condition?
    condition-predicate
    condition-accessor
    define-condition-type
    ;; Standard condition types
    &message
    make-message-condition
    message-condition?
    condition-message
    &warning
    make-warning-condition
    warning-condition?
    &serious
    make-serious-condition
    serious-condition?
    &error
    make-error
    error?
    &violation
    make-violation
    violation?
    &irritants
    make-irritants-condition
    irritants-condition?
    condition-irritants
    &who
    make-who-condition
    who-condition?
    condition-who
    &non-continuable
    make-non-continuable-violation
    non-continuable-violation?
    &implementation-restriction
    make-implementation-restriction-violation
    implementation-restriction-violation?
    &lexical
    make-lexical-violation
    lexical-violation?
    &syntax
    make-syntax-violation
    syntax-violation?
    syntax-violation-form
    syntax-violation-subform
    &undefined
    make-undefined-violation
    undefined-violation?)
  (import
    ($system)
    ($condition-names)
    (rnrs records syntactic)
    (scheme-libraries define-who))

  (define-condition-name &condition (condition-rtd))

  (define-syntax/who define-condition-type
    (lambda (x)
      (syntax-case x ()
        [(_ condition-type super-type
            constructor predicate
            (field accessor) ...)
         (for-all identifier? #'(condition-type super-type constructor predicate field ... accessor ...))
         #'(define-record-type (condition-type constructor predicate)
             (parent super-type)
             (fields (immutable field accessor) ...))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-condition-name &message (message-rtd))
  (define-condition-name &warning (warning-rtd))
  (define-condition-name &serious (serious-rtd))
  (define-condition-name &error (error-rtd))
  (define-condition-name &violation (violation-rtd))
  (define-condition-name &irritants (irritants-rtd))
  (define-condition-name &who (who-rtd))
  (define-condition-name &non-continuable (non-continuable-rtd))
  (define-condition-name &implementation-restriction (implementation-restriction-rtd))
  (define-condition-name &lexical (lexical-rtd))
  (define-condition-name &syntax (syntax-rtd))
  (define-condition-name &undefined (undefined-rtd))


  )
