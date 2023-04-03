#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries heaps)
  (export
    make-heap
    heap?
    heap-ordering-predicate
    heap-push!
    heap-pop!
    heap-top
    heap-size
    heap-empty?)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries numbers)
    (scheme-libraries record-writer))

  (define-record-type heap
    (nongenerative heap-4d138d64-7e48-471e-9210-de5fc877be0b)
    (sealed #t)
    (fields
      ordering-predicate
      (mutable size)
      (mutable elements))
    (protocol
      (lambda (new)
        (define who 'make-heap)
        (define make
          (case-lambda
            [(ordering-predicate k)
             (unless (procedure? ordering-predicate)
               (assertion-violation who "invalid ordering predicate argument" ordering-predicate))
             (unless (exact-nonnegative-integer? k)
               (assertion-violation who "invalid initial capacity argument" k))
             (new ordering-predicate 0 (make-vector k))]
            [(ordering-predicate)
             (make ordering-predicate 10)]))
        make)))

  (define/who heap-push!
    (lambda (heap el)
      (unless (heap? heap)
        (assertion-violation who "invalid heap argument" heap))
      (let* ([size (heap-size heap)]
             [elements (heap-elements heap)]
             [capacity (vector-length elements)])
        (when (fx=? size (vector-length elements))
          (let ([new-elements (make-vector (* 2 capacity))])
            (do ([i 0 (fx+ i 1)])
                ((fx=? i size))
              (vector-set! new-elements i (vector-ref elements i)))
            (heap-elements-set! heap new-elements)))
        (let ([elements (heap-elements heap)]
              [<? (heap-ordering-predicate heap)])
          (vector-set! elements size el)
          (heap-size-set! heap (fx+ size 1))
          (let f! ([i size])
            (unless (fxzero? i)
              (let ([j (parent-index i)])
                (let ([parent-el (vector-ref elements j)])
                  (when (<? el parent-el)
                    (vector-set! elements i parent-el)
                    (vector-set! elements j el)
                    (f! j))))))))))

  (define/who heap-pop!
    (lambda (heap)
      (unless (heap? heap)
        (assertion-violation who "invalid heap argument" heap))
      (let ([size (heap-size heap)])
        (when (fxzero? size)
          (assertion-violation who "heap empty" heap))
        (let* ([elements (heap-elements heap)]
               [top-el (vector-ref elements 0)]
               [size (fx- size 1)]
               [el (vector-ref elements size)])
          (vector-set! elements 0 el)
          (heap-size-set! heap size)
          (let ([<? (heap-ordering-predicate heap)])
            (let f! ([i 0])
              (let* ([left (left-index i)]
                     [right (right-index i)]
                     [j (if (and (fx<? left size)
                                 (<? (vector-ref elements left)
                                     el))
                            left
                            i)]
                     [j (if (and (fx<? right size)
                                 (<? (vector-ref elements right)
                                     (vector-ref elements j)))
                            right
                            j)])
                (unless (fx=? i j)
                  (vector-set! elements i (vector-ref elements j))
                  (vector-set! elements j el)
                  (f! j)))))
          top-el))))

  (define/who heap-empty?
    (lambda (heap)
      (unless (heap? heap)
        (assertion-violation who "invalid heap argument" heap))
      (fxzero? (heap-size heap))))

  (define/who heap-top
    (lambda (heap)
      (unless (heap? heap)
        (assertion-violation who "invalid heap argument" heap))
      (when (fxzero? (heap-size heap))
        (assertion-violation who "heap empty" heap))
      (vector-ref (heap-elements heap) 0)))

  (define parent-index
    (lambda (i)
      (fxdiv (fx- i 1) 2)))

  (define left-index
    (lambda (i)
      (fx+ (fx* i 2) 1)))

  (define right-index
    (lambda (i)
      (fx+ (fx* i 2) 2)))

  ;; Record writers

  (record-writer (record-type-descriptor heap)
    (lambda (r p wr)
      (put-string p "#<heap ")
      (wr
       (let ([size (heap-size r)]
             [elements (heap-elements r)])
         (let f ([i 0])
           (if (fx<? i size)
               (cons (vector-ref elements i)
                     (f (fx+ i 1)))
               '())))
       p)
      (put-string p ">")))
  )
