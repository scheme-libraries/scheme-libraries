#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries assembly-output $target)
  (export
    label->string
    operand->string
    target->string
    emit-func-directive
    emit-epilog)
  (import
    (rnrs)
    (scheme-libraries assembly-output $common)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries gensyms)
    (scheme-libraries match))

  (define register?
    (lambda (op)
      (memq op '(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))))

  (define label?
    (lambda (x)
      (or (string? x)
          (symbol? x))))

  (define/who operand->string
    (lambda (op)
      (match op
        [(disp ,reg 0)
         (guard (register? reg))
         (format "(%~a)" reg)]
        [(disp ,reg ,disp)
         (guard (register? reg) (number? disp))
         (format "~a(%~a)" disp reg)]
        [(index 1 ,index ,base)
         (format "(%~a,%~a)" base index)]
        [(index ,scale ,index ,base)
         (format "(%~a,%~a,%~a)" base index scale)]
        [(disp ,label 0)
         (guard (label? label))
         (format "~a(%rip)" (label->string label))]
        [(disp ,label ,disp)
         (guard (label? label) (number? disp))
         (format "~a+~a(%rip)" disp (label->string label))]
        [(abs ,disp)
         (format "~a" disp)]
        [,imm
         (guard (number? imm))
         (format "$~a" imm)]
        [,reg
         (guard (register? reg))
         (format "%~a" reg)]
        [,label
         (guard (label? label))
         (format "~a(%rip)" (label->string label))]
        [,_
         (assertion-violation who "invalid operand argument" op)])))

  (define/who target->string
    (lambda (op)
      (match op
        [(disp ,reg 0)
         (guard (register? reg))
         (format "*(%~a)" reg)]
        [(disp ,reg ,disp)
         (guard (register? reg) (number? disp))
         (format "*~a(%~a)" disp reg)]
        [(disp ,label 0)
         (guard (label? label))
         (format "~a(%rip)" (label->string label))]
        [(disp ,label ,disp)
         (guard (label? label) (number? disp))
         (format "~a+~a(%rip)" disp (label->string label))]
        [,imm
         (guard (number? imm))
         (format "~a" imm)]
        [,reg
         (guard (register? reg))
         (format "*%~a" reg)]
        [,label
         (guard (label? label))
         (label->string label)]
        [,_
         (assertion-violation who "invalid operand argument" op)])))

  (define/who label->string
    (lambda (label)
      (cond
       [(string? label) label]
       [(symbol? label) (format ".L~a" (gensym-suffix label))]
       [else
        (assertion-violation who "invalid label argument" label)])))

  (define emit-func-directive
    (lambda (label)
      (put-string (assembly-output-port)
                  (format "\t.type\t~a, @function~%"
                          (label->string label)))))

  (define emit-epilog
    (lambda ()
      (put-string (assembly-output-port)
                  "\t.section\t.note.GNU-stack, \"\", @progbits\n"))))
