#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries trees)
  (export
    tree->list)
  (import
    (rnrs))

  (define tree->list
    (lambda (tree)
      (let f ([tree tree]
              [tail '()])
        (cond [(null? tree)
               tail]
              [(pair? tree)
               (f (car tree)
                  (f (cdr tree)
                     tail))]
              [else (cons tree tail)])))))
