#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries graph-coloring)
  (export
    color?
    node
    node?
    node=?
    node-color
    make-graph
    graph?
    graph-colored?
    graph-add-interference!
    graph-add-move!
    color-graph!)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries heaps)
    (scheme-libraries numbers)
    (scheme-libraries record-writer)
    (scheme-libraries worklists))

  ;; For the algorithm used, see for example, A. Appel: Modern
  ;; Compiler Implementation in C, chapter 11.

  ;; Colors

  (define color?
    (lambda (obj)
      (exact-nonnegative-integer? obj)))

  ;; Nodes

  (define-record-type node
    (nongenerative node-cf91f967-9404-4e76-b3de-39895da2db3d)
    (parent element)
    (fields
      spill-cost
      index
      (mutable adjacency-set)
      (mutable adjacency-list)
      (mutable move-list)
      (mutable degree)
      (mutable color)
      (mutable alias)
      (mutable seen?)
      (mutable spill-priority))
    (protocol
      (lambda (pargs->new)
        (define who 'make-node)
        (lambda (graph cost color)
          (unless (graph? graph)
            (assertion-violation who "invalid graph argument" graph))
          (unless (real? cost)
            (assertion-violation who "invalid cost argument" cost))
          (unless (or (not color) (color? color))
            (assertion-violation who "invalid color argument" color))
          (let* ([index (graph-node-count! graph)]
                 [new (pargs->new)]
                 [node (new cost
                            index
                            (make-adjacency-set)
                            '()
                            '()
                            0
                            color
                            #f
                            #f
                            #f)])
            (worklist-add! (if color
                               (graph-precolored graph)
                               (graph-initial graph))
                           node)
            node)))))

  (define/who node=?
    (lambda (x y)
      (unless (node? x)
        (assertion-violation who "invalid first node argument" x))
      (unless (node? y)
        (assertion-violation who "invalid first node argument" y))
      (eq? x y)))

  (define precolored?
    (lambda (node graph)
      (worklist=? (element-worklist node)
                  (graph-precolored graph))))

  (define node-adjacency-list-add!
    (lambda (node1 node2)
      (node-adjacency-list-set! node1
                                (cons node2
                                      (node-adjacency-list node1)))))

  (define node-degree-incr!
    (lambda (node)
      (node-degree-set! node (fx+ (node-degree node) 1))))

  (define node-degree-decr!
    (lambda (node)
      (node-degree-set! node (fx- (node-degree node) 1))))

  (define node-move-list-add!
    (lambda (node move)
      (node-move-list-set! node
                           (cons move (node-move-list node)))))

  (define node-move-list-union!
    (lambda (node1 node2)
      (node-move-list-set! node1
                           (move-list-union (node-move-list node1)
                                            (node-move-list node2)))))

  (define node-list-union
    (lambda (node-list1 node-list2)
      (let ([nodes
             (fold-left
              (lambda (nodes node)
                (cond
                 [(node-seen? node) nodes]
                 [else
                  (node-seen?-set! node #t)
                  (cons node nodes)]))
              node-list1 node-list2)])
        (for-each (lambda (node)
                    (node-seen?-set! node #f))
                  node-list2)
        nodes)))

  ;; Moves

  (define-record-type move
    (nongenerative move-380b8aef-5b25-43a3-ab39-4b08f1d351fd)
    (parent element)
    (sealed #t)
    (fields
      use
      def
      (mutable seen?))
    (protocol
      (lambda (pargs->new)
        (lambda (use def)
          ((pargs->new) use def #f)))))

  (define move=?
    (lambda (move1 move2)
      (and (node=? (move-use move1)
                   (move-use move2))
           (node=? (move-def move1)
                   (move-def move2)))))

  (define move-list-contains?
    (lambda (move-list move)
      (and (memp (lambda (x)
                   (move=? x move))
                 move-list)
           #t)))

  (define move-list-union
    (lambda (move-list1 move-list2)
      (let ([moves
             (fold-left
              (lambda (moves move)
                (cond
                 [(move-seen? move) moves]
                 [else
                  (move-seen?-set! move #t)
                  (cons move moves)]))
              move-list1 move-list2)])
        (for-each (lambda (move)
                    (move-seen?-set! move #f))
                  move-list2)
        moves)))

  ;; Graphs

  (define-record-type graph
    (nongenerative graph-16dc2a5b-4385-4df4-ad19-4f0237ee0155)
    (sealed #t)
    (fields
      (mutable colored?)
      (mutable node-count)
      precolored
      initial
      worklist-moves)
    (protocol
      (lambda (new)
        (lambda ()
          (new #f
               0
               (make-worklist)
               (make-worklist)
               (make-worklist))))))

  (define graph-node-count!
    (lambda (graph)
      (let ([count (graph-node-count graph)])
        (graph-node-count-set! graph (+ count 1))
        count)))

  (define/who graph-add-interference!
    (lambda (graph live def)
      (unless (graph? graph)
        (assertion-violation who "invalid graph argument" graph))
      (unless (node? live)
        (assertion-violation who "invalid live argument" live))
      (unless (node? def)
        (assertion-violation who "invalid def argument" def))
      (unless (adjacent? live def)
        (adjacent! live def)
        (unless (precolored? live graph)
          (node-adjacency-list-add! live def)
          (node-degree-incr! live))
        (unless (precolored? def graph)
          (node-adjacency-list-add! def live)
          (node-degree-incr! def)))))

  (define/who graph-add-move!
    (lambda (graph use def)
      (unless (graph? graph)
        (assertion-violation who "invalid graph argument" graph))
      (unless (node? use)
        (assertion-violation who "invalid live argument" use))
      (unless (node? def)
        (assertion-violation who "invalid def argument" def))
      (unless (or (node=? use def)
                  (and (precolored? use graph)
                       (precolored? def graph)))
        (let-values ([(u v)
                      (if (or (precolored? use graph)
                              (fx>? (node-index use)
                                    (node-index def)))
                          (values def use)
                          (values use def))])
          (let ([move (make-move u v)])
            (unless (move-list-contains? (node-move-list u) move)
              (worklist-add! (graph-worklist-moves graph) move)
              (node-move-list-add! use move)
              (node-move-list-add! def move)))))))

  ;; Adjacency sets

  (define make-adjacency-set
    (lambda ()
      0))

  (define adjacent?
    (lambda (live def)
      (bitwise-bit-set? (node-adjacency-set live)
                        (node-index def))))

  (define adjacent!
    (lambda (live def)
      (node-adjacency-set-set! live
                               (bitwise-copy-bit (node-adjacency-set live) (node-index def) 1))
      (node-adjacency-set-set! def
                               (bitwise-copy-bit (node-adjacency-set def) (node-index live) 1))))

  ;; Graph coloring

  (define/who color-graph!
    (lambda (graph k)
      (unless (graph? graph)
        (assertion-violation who "invalid graph argument" graph))
      (unless (exact-nonnegative-integer? k)
        (assertion-violation "invalid color count argument" k))
      (when (graph-colored? graph)
        (assertion-violation who "graph already colored" graph))
      (docolor! graph k)))

  (define docolor!
    (lambda (graph k)
      ;; We may not need all worklists, e.g. the colored-nodes or
      ;; spilled-nodes.
      (let ([worklist-moves (graph-worklist-moves graph)]
            [simplify-worklist (make-worklist)]
            [freeze-worklist (make-worklist)]
            [spill-worklist (make-worklist)]
            [spilled-nodes (make-worklist)]
            [coalesced-nodes (make-worklist)]
            [colored-nodes (make-worklist)]
            [select-stack (make-worklist)]
            [coalesced-moves (make-worklist)]
            [constrained-moves (make-worklist)]
            [frozen-moves (make-worklist)]
            [active-moves (make-worklist)]
            [spill-heap (make-heap (lambda (x y)
                                     (< (node-spill-priority x)
                                        (node-spill-priority y))))])

        (define ok?
          (lambda (t r)
            (or (low-degree? t)
                (precolored? t graph)
                (adjacent? t r))))

        (define colored?
          (lambda (node)
            (worklist=? (element-worklist node)
                        colored-nodes)))

        (define coalesced?
          (lambda (node)
            (worklist=? (element-worklist node) coalesced-nodes)))

        (define frozen?
          (lambda (node)
            (worklist=? (element-worklist node) freeze-worklist)))

        (define active-move?
          (lambda (move)
            (worklist=? (element-worklist move) active-moves)))

        (define worklist-move?
          (lambda (move)
            (worklist=? (element-worklist move) worklist-moves)))

        (define move-related?
          (lambda (node)
            (find (lambda (move)
                    (or (active-move? move)
                        (worklist-move? move)))
                  (node-move-list node))))

        (define high-degree?
          (lambda (node)
            (fx>=? (node-degree node) k)))

        (define low-degree?
          (lambda (node)
            (fx<? (node-degree node) k)))

        (define conservative?
          (lambda (node*)
            (fx<? (fold-left (lambda (i node)
                               (if (high-degree? node)
                                   (fx+ i 1)
                                   i))
                             0 node*)
                  k)))

        (define node-for-each-active-move
          (lambda (proc node)
            (for-each
             (lambda (move)
               (when (active-move? move)
                 (proc move)))
             (node-move-list node))))

        (define node-for-each-adjacency
          (lambda (proc node)
            (for-each
             (lambda (node)
               (let ([worklist (element-worklist node)])
                 (unless (or (worklist=? worklist select-stack)
                             (worklist=? worklist coalesced-nodes))
                   (proc node))))
             (node-adjacency-list node))))

        (define initialize-worklists!
          (lambda ()
            (worklist-for-each
             (lambda (node)
               (node-spill-priority-set! node
                                         (/ (node-spill-cost node)
                                            (node-degree node)))
               (when (high-degree? node)
                 (heap-push! spill-heap node))
               (worklist-add! (cond
                               [(high-degree? node) spill-worklist]
                               [(move-related? node) freeze-worklist]
                               [else simplify-worklist])
                              node))
             (graph-initial graph))))

        (define decrement-degree!
          (lambda (node)
            (let ([deg (node-degree node)])
              (node-degree-decr! node)
              (when (fx=? deg k)
                (enable-moves! (cons node (node-adjacency-list node)))
                (worklist-add! (if (move-related? node)
                                   freeze-worklist
                                   simplify-worklist)
                               node)))))

        (define combine!
          (lambda (u v)
            (worklist-add! coalesced-nodes v)
            (node-alias-set! v u)
            (node-move-list-union! u v)
            (enable-moves! (list v))
            (node-for-each-adjacency
             (lambda (t)
               (graph-add-interference! graph t u)
               (decrement-degree! t))
             v)
            (when (and (high-degree? u)
                       (frozen? u))
              (worklist-add! spill-worklist u)
              (heap-push! spill-heap u))))

        (define enable-moves!
          (lambda (node*)
            (for-each
             (lambda (node)
               (node-for-each-active-move
                (lambda (move)
                  (worklist-add! worklist-moves move))
                node))
             node*)))

        (define freeze-moves!
          (lambda (node)
            (node-for-each-active-move
             (lambda (move)
               (let ([x (move-use move)]
                     [y (move-def move)])
                 (let ([v (ref-alias (if (node=? (ref-alias y)
                                                 (ref-alias node))
                                         x
                                         y))])
                   (worklist-add! frozen-moves move)
                   (when (and (move-related? v)
                              (low-degree? v))
                     (worklist-add! simplify-worklist v)))))
             node)))

        (define ref-alias
          (lambda (node)
            (if (coalesced? node)
                (ref-alias (node-alias node))
                node)))

        (define simplify!
          (lambda ()
            (let ([node (worklist-first simplify-worklist)])
              (worklist-add! select-stack node)
              (node-for-each-adjacency decrement-degree! node))))

        (define coalesce!
          (lambda ()
            (let ([move (worklist-first worklist-moves)])
              (let ([x (ref-alias (move-use move))]
                    [y (ref-alias (move-def move))])
                (let-values ([(u v)
                              (if (precolored? y graph)
                                  (values y x)
                                  (values x y))])
                  (cond
                   [(node=? u v)
                    (worklist-add! coalesced-moves move)
                    (add-worklist! u)]
                   [(or (precolored? v graph)
                        (adjacent? u v))
                    (worklist-add! constrained-moves move)
                    (add-worklist! u)
                    (add-worklist! v)]
                   [(if (precolored? u graph)
                        (for-all (lambda (t)
                                   (ok? t u))
                                 (node-adjacency-list u))
                        (conservative? (node-list-union (node-adjacency-list u)
                                                        (node-adjacency-list v))))
                    (worklist-add! coalesced-moves move)
                    (combine! u v)
                    (add-worklist! u)]
                   [else
                    (worklist-add! active-moves move)]))))))

        (define add-worklist!
          (lambda (node)
            (when (and (not (precolored? node graph))
                       (not (move-related? node))
                       (low-degree? node))
            (worklist-add! simplify-worklist node))))

        (define freeze!
          (lambda ()
            (let ([node (worklist-first freeze-worklist)])
              (worklist-add! simplify-worklist node)
              (freeze-moves! node))))

        (define select-spill!
          (lambda ()
            (let f! ()
              (let ([node (heap-pop! spill-heap)])
                (unless (worklist=? (element-worklist node)
                                    spill-worklist)
                  (f!))
                (worklist-add! simplify-worklist node)
                (freeze-moves! node)))))

        (define assign-colors!
          (lambda ()
            (worklist-for-each
             (lambda (node)
               (let ([ok-colors
                      (fold-left
                       (lambda (ok-colors node)
                         (if (or (colored? node)
                                 (precolored? node graph))
                             (bitwise-copy-bit ok-colors
                                               (node-color (ref-alias node))
                                               0)
                             ok-colors))
                       (- (bitwise-arithmetic-shift-left 1 k) 1)
                       (node-adjacency-list node))])
                 (cond
                  [(zero? ok-colors)
                   (worklist-add! spilled-nodes node)]
                  [else
                   (worklist-add! colored-nodes node)
                   (node-color-set! node (bitwise-first-bit-set ok-colors))])))
             select-stack)
            (worklist-for-each
             (lambda (node)
               (node-color-set! node (node-color (ref-alias node))))
             coalesced-nodes)))

        (initialize-worklists!)
        (let loop! ()
          (cond
           [(not (worklist-empty? simplify-worklist))
            (simplify!)
            (loop!)]
           [(not (worklist-empty? worklist-moves))
            (coalesce!)
            (loop!)]
           [(not (worklist-empty? freeze-worklist))
            (freeze!)
            (loop!)]
           [(not (worklist-empty? spill-worklist))
            (select-spill!)
            (loop!)]))
        (assign-colors!))
      (graph-colored?-set! graph #t)))

  ;; Record writers

  (record-writer (record-type-descriptor node)
    (lambda (r p wr)
      (put-string p "#<node>")))

  (record-writer (record-type-descriptor graph)
    (lambda (r p wr)
      (put-string p "#<graph>"))))
