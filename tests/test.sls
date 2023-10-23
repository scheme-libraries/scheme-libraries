(library (test)
  (export
    foo
    bar)
  (import
    (rnrs)
    (test2))

  (define-syntax foo
    (lambda (x)
      (syntax-case x ()
        [(_)
         (id #'y)])))

  (define y (id 12))

  (define-record-type bar)

  )
