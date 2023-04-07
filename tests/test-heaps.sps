#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries heaps)
        (scheme-libraries random-numbers)
        (scheme-libraries testing))

(define heapify
  (lambda (< ls)
    (let ([heap (make-heap < (length ls))])
      (for-each
       (lambda (el)
         (heap-push! heap el))
       ls)
      heap)))

(define heap->list
  (lambda (heap)
    (let f! ()
      (if (heap-empty? heap)
          '()
          (let ([top (heap-pop! heap)])
            (cons top (f!)))))))

(test-begin "heaps")

(test-assert (heap? (make-heap <)))
(test-assert (heap? (make-heap < 100)))
(test-assert (let ([< (heap-ordering-predicate (make-heap <))])
               (< 1 2)))
(test-assert (heap-empty? (make-heap <)))
(test-eqv 0 (heap-size (make-heap <)))

(test-assert (not (heap-empty? (heapify < '(1 3 2)))))
(test-eqv 3 (heap-size (heapify < '(1 3 2))))

(test-eqv 2 (heap-top (heapify < '(10 3 2))))

(test-equal '(1 2 3 4 5 6 7 8 9 10)
  (heap->list (heapify < '(5 8 9 1 3 4 10 2 7 6))))

(define x* (do ([n 0 (fx+ n 1)]
                [x* '() (cons (random 100) x*)])
               ((fx=? n 100)
                x*)))

(test-equal (list-sort fx<=? x*) (heap->list (heapify fx<? x*)))

(test-end "heaps")
