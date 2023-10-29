(library (test)
  (export
    foo
    bar
    fruit)
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

  (define *store* 'apple)

  (define (fruit) *store*)

  (set! *store* 'cherry)

  )
