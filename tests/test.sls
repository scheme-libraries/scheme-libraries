(library (test)
  (export foo)
  (import
    ($system))

  (define-syntax foo
    (lambda (x)
      12))

  )
