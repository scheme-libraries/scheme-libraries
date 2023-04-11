#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries atoms)
  (export
    atom?
    atom=?)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define atom?
    (lambda (obj)
      (or (boolean? obj)
          (bytevector? obj)
          (char? obj)
          (null? obj)
          (number? obj)
          (symbol? obj)
          (string? obj))))

  (define/who atom=?
    (lambda (x y)
      (unless (atom? x)
        (assertion-violation who "invalid first atom argument" x))
      (unless (atom? y)
        (assertion-violation who "invalid first atom argument" y))
      (or (eqv? x y)
          (cond
           [(string? x)
            (and (string? y) (string=? x y))]
           [(bytevector? x)
            (and (bytevector? y)
                 (bytevector=? x y))]))))
  )
