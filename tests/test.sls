(library (test)
  (export foo)
  (import
    ($system)
    (test2))

  (define-syntax foo
    (lambda (x)
      (id #'y)))

  (define y 12)

  )
