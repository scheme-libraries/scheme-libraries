#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries assembly-output $target)
  (export
    label->string
    operand->string)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries gensyms))

  (define register?
    (lambda (op)
      (memq op '(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))))

  (define/who operand->string
    (lambda (op)
      (cond
       [(number? op) (format "$~s" op)]
       [(register? op) (format "%~s" op)]
       [else
        (assertion-violation who "invalid operand argument" op)])))

  (define/who label->string
    (lambda (label)
      (cond
       [(string? label) label]
       [(symbol? label) (format ".L~s" (gensym-suffix label))]
       [else
        (assertion-violation who "invalid label argument" label)])))
  )
