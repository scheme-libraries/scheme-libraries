#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries testing)
  (scheme-libraries continuation-prompts))

(test-begin "continuation prompts")

(test-assert (continuation-prompt-tag? (make-continuation-prompt-tag)))
(test-assert (continuation-prompt-tag? (make-continuation-prompt-tag 'tag)))
(test-assert (not (equal? (make-continuation-prompt-tag)
                          (make-continuation-prompt-tag))))

(define tag (make-continuation-prompt-tag 'tag))

(test-eqv 1
  (call-with-continuation-prompt
    (lambda ()
      1)
    tag))

(test-end "continuation prompts")
