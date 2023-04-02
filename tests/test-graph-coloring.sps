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

(test-end "graph coloring")
