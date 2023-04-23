(library (test)
  (export foo)
  (import
    ($system))

  (define-syntax foo
    (lambda (x)
      #'y))

  (define y 12)

  )
