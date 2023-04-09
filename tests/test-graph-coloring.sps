#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import
  (rnrs)
  (scheme-libraries define-values)
  (scheme-libraries testing)
  (scheme-libraries record-writer)
  (scheme-libraries graph-coloring))

(define-record-type temporary
  (nongenerative temporary-89fddabe-a456-4ebc-a9ef-e2fe39564c9c)
  (parent node) (sealed #t)
  (fields
    name)
  (protocol
   (lambda (pargs->new)
     (lambda (graph cost color name)
       ((pargs->new graph cost color) name)))))

(record-writer (record-type-descriptor temporary)
  (lambda (r p wr)
    (put-string p "#<temporary ")
    (wr (temporary-name r) p)
    (put-string p " color: ")
    (wr (node-color r) p)
    (put-string p ">")))

(test-begin "graph coloring")

(define graph (make-graph))

(test-assert (graph? graph))

(define tmp-r (make-temporary graph 0 0 'r))
(define tmp-x (make-temporary graph 1 #f 'x))
(define tmp-y (make-temporary graph 10 #f 'y))

(test-assert (temporary? tmp-r))
(test-assert (temporary? tmp-x))
(test-assert (temporary? tmp-y))

(graph-add-interference! graph tmp-x tmp-y)
(graph-add-move! graph tmp-r tmp-x)
(graph-add-move! graph tmp-r tmp-y)

(test-assert (not (graph-colored? graph)))

(define-values (colored spilled)
  (color-graph! graph 1))

(test-assert (graph-colored? graph))

(test-eqv #f (node-color tmp-x))
(test-eqv 0 (node-color tmp-y))

(test-equal (list tmp-y) colored)
(test-equal (list tmp-x) spilled)

(let ()
  (define graph (make-graph))
  (define rax (make-temporary graph 0.0 0 'rax))
  (define rbp (make-temporary graph 0.0 1 'rbp))
  (define r8 (make-temporary graph 0.0 2 'r8))
  (define r15 (make-temporary graph 0.0 3 'r15))
  (define t.4 (make-temporary graph 0.0 #f 't.4))
  (define p.2 (make-temporary graph 0.0 #f 'p.2))
  (define t.3 (make-temporary graph 0.0 #f 't.3))
  (define rp.6 (make-temporary graph 0.0 #f 'rp.6))
  (define t.7 (make-temporary graph +inf.0 #f 't.7))
  (graph-add-interference! graph rp.6 t.7)
  (graph-add-interference! graph rp.6 rbp)
  (graph-add-interference! graph rp.6 r8)
  (graph-add-interference! graph rp.6 p.2)
  (graph-add-interference! graph rp.6 t.4)
  (graph-add-interference! graph rp.6 t.3)
  (graph-add-interference! graph rp.6 rax)
  (graph-add-interference! graph t.3 p.2)
  (graph-add-interference! graph t.3 t.4)
  (graph-add-interference! graph t.3 rbp)
  (graph-add-interference! graph t.3 rp.6)
  (graph-add-interference! graph t.4 t.7)
  (graph-add-interference! graph t.4 rbp)
  (graph-add-interference! graph t.4 p.2)
  (graph-add-interference! graph t.4 rp.6)
  (graph-add-interference! graph t.4 t.3)
  (graph-add-interference! graph p.2 rbp)
  (graph-add-interference! graph p.2 t.4)
  (graph-add-interference! graph p.2 rp.6)
  (graph-add-interference! graph p.2 t.3)
  (graph-add-interference! graph t.7 rbp)
  (graph-add-interference! graph t.7 t.4)
  (graph-add-interference! graph t.7 rp.6)
  (graph-add-move! graph t.3 t.7)
  (graph-add-move! graph rp.6 r15)
  (graph-add-move! graph p.2 r8)
  (graph-add-move! graph t.7 t.3)
  (graph-add-move! graph t.7 rax)
  (color-graph! graph 10)
  (test-assert (not (eqv? (node-color t.3)
                          (node-color t.4)))))

(test-end "graph coloring")
