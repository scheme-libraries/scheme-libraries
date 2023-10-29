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

  (define fruit 'apple)

  ;; FIXME: The following line should raise an exception because an
  ;; exported variable must not be mutated.
  #;
  (set! fruit 'cherry)

  )
