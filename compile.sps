#!r6rs

(import (chezscheme))

(define name (cadr (command-line)))

(compile-file-message #f)
(compile-imported-libraries #t)
(generate-wpo-files #t)
(compile-program (string-append name ".sps")
                 (string-append name ".so"))
(compile-whole-program (string-append name ".wpo")
                       name
                       #t)
