#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries reading positions)
  (export
    make-position
    position?
    position-line
    position-column
    position-lines
    position-columns
    position-tabulator
    position->s-expr
    s-expr->position)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries match)
    (scheme-libraries numbers))

  (define-record-type position
    (nongenerative position-6bb63d2d-bc96-4600-8008-fb12be0fa23f)
    (sealed #t) (opaque #t)
    (fields line column)
    (protocol
      (lambda (new)
        (define who 'make-position)
        (case-lambda
          [(line column)
           (unless (and (fixnum? line) (positive? line))
             (assertion-violation who "invalid line argument" line))
           (unless (and (fixnum? column) (positive? column))
             (assertion-violation who "invalid column argument" column))
           (new line column)]
          [() (new 1 1)]))))

  (define/who position-lines
    (lambda (position lines)
      (unless (position? position)
        (assertion-violation who "invalid position argument" position))
      (unless (nonnegative-fixnum? lines)
        (assertion-violation who "invalid lines argument" lines))
      (make-position (fx+ (position-line position) lines)
                     1)))

  (define/who position-columns
    (lambda (position columns)
      (unless (position? position)
        (assertion-violation who "invalid position argument" position))
      (unless (nonnegative-fixnum? columns)
        (assertion-violation who "invalid columns argument" columns))
      (make-position (position-line position)
                     (fx+ (position-column position) columns))))

  (define/who position-tabulator
    (lambda (position)
      (unless (position? position)
        (assertion-violation who "invalid position argument" position))
      (make-position (position-line position)
                     (fx+ (fxand (fx+ (position-column position) 7) -8) 1))))

  (define/who position->s-expr
    (lambda (pos)
      (unless (position? pos)
        (assertion-violation who "invalid position argument" pos))
      `#(,(position-line pos) ,(position-column pos))))

  (define/who s-expr->position
    (lambda (e)
      (match e
        [#(,line ,col) (make-position line col)]
        [,e (assertion-violation who "invalid position datum argument" e)])))
  )
