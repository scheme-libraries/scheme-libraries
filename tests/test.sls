(library (test)
  (export foo)
  (import
    (rnrs)
    (test2))

  (define-syntax foo
    (lambda (x)
      (id #'y)))

  (define y (id 12))

  )
