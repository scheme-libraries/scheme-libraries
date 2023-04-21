#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries continuation-prompts)
  (export
    make-continuation-prompt-tag
    continuation-prompt-tag?
    call-with-continuation-prompt
    abort-current-continuation
    &continuation
    make-continuation-violation
    continuation-violation?
    continuation-violation-prompt-tag)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries parameters)
    (scheme-libraries rec))

  ;; Prompt tags

  (define-record-type continuation-prompt-tag
    (nongenerative continuation-prompt-tag-9a566238-e793-430b-8a18-1c08e2ce1fa4)
    (fields name parameter)
    (protocol
      (lambda (new)
        (define who 'make-prompt-tag)
        (case-lambda
          [(name)
           (unless (symbol? name)
             (assertion-violation who "invalid name argument" name))
           (new name (make-parameter #f))]
          [()
           (new #f (make-parameter #f))]))))

  ;; Continuation prompts

  (define/who call-with-continuation-prompt
    (case-lambda
      [(thunk prompt-tag abort-handler)
       (unless (procedure? thunk)
         (assertion-violation who "invalid thunk argument" thunk))
       (unless (continuation-prompt-tag? prompt-tag)
         (assertion-violation who "invalid prompt tag argument" prompt-tag))
       (unless (procedure? abort-handler)
         (assertion-violation who "invalid handler argument" abort-handler))
       ((call/cc
         (lambda (abort)
           (parameterize ([(continuation-prompt-tag-parameter prompt-tag)
                           (lambda (obj*)
                             (abort
                              (lambda ()
                                (apply abort-handler obj*))))])
             (call-with-values thunk
               (lambda obj*
                 (lambda () (apply values obj*))))))))]
      [(thunk prompt-tag)
       (call-with-continuation-prompt
         thunk prompt-tag
         (rec f
           (lambda (thunk)
             (call-with-continuation-prompt thunk prompt-tag f))))]))

  (define/who abort-current-continuation
    (lambda (prompt-tag . obj*)
      (unless (continuation-prompt-tag? prompt-tag)
        (assertion-violation who "invalid prompt tag" prompt-tag))
      (let ([handler ((continuation-prompt-tag-parameter prompt-tag))])
        (unless handler
          (let ([name (continuation-prompt-tag-name prompt-tag)])
            (raise (condition
                     (make-violation)
                     (make-who-condition who)
                     (make-message-condition
                      (if name
                          (format "prompt tag ~a not found in the continuation" name)
                          "prompt tag not found in the continuation"))
                     (make-irritants-condition (cons prompt-tag obj*))
                     (make-continuation-violation prompt-tag)))))
        (handler obj*))))

  ;; Conditions

  (define-condition-type &continuation &violation
    make-continuation-violation continuation-violation?
    (prompt-tag continuation-violation-prompt-tag))



  )
