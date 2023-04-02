#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries worklists)
  (export
    element
    element?
    element-worklist
    make-worklist
    worklist=?
    worklist?
    worklist-empty?
    worklist-first
    worklist-add!
    worklist-for-each
    element-remove!
    worklist->list)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries record-writer))

  (define-record-type element
    (nongenerative element-14b1e575-1196-4acb-85c7-25a363036dae)
    (fields
      (mutable worklist)
      (mutable previous)
      (mutable next))
    (protocol
     (lambda (new)
       (lambda ()
         (new #f #f #f)))))

  (define-record-type worklist
    (nongenerative worklist-e15f08f9-239e-441d-b1fc-14a6fc7b3345)
    (sealed #t)
    (fields
      (mutable first))
    (protocol
     (lambda (new)
       (lambda ()
         (new #f)))))

  (define/who worklist-empty?
    (lambda (wl)
      (unless (worklist? wl)
        (assertion-violation who "invalid worklist argument" wl))
      (not (worklist-first wl))))

  (define/who worklist=?
    (lambda (wl1 wl2)
      (unless (worklist? wl1)
        (assertion-violation who "invalid first worklist argument" wl1))
      (unless (worklist? wl1)
        (assertion-violation who "invalid second worklist argument" wl2))
      (eq? wl1 wl2)))

  (define/who worklist-add!
    (lambda (worklist element)
      (unless (worklist? worklist)
        (assertion-violation who "invalid worklist argument" worklist))
      (unless (element? element)
        (assertion-violation who "invalid element argument" element))
      (element-remove! element)
      (element-worklist-set! element worklist)
      (cond
       [(worklist-first worklist)
        => (lambda (first)
             (element-next-set! element first)
             (element-previous-set! first element))])
      (worklist-first-set! worklist element)))

  (define/who worklist-for-each
    (lambda (proc worklist)
      (unless (procedure? proc)
        (assertion-violation who "invalid procedure argument" proc))
      (unless (worklist? worklist)
        (assertion-violation who "invalid worklist argument" worklist))
      (let f ([element (worklist-first worklist)])
        (when element
          (let ([next (element-next element)])
            (proc element)
            (f next))))))

  (define/who worklist->list
    (lambda (worklist)
      (unless (worklist? worklist)
        (assertion-violation who "invalid worklist argument" worklist))
      (let f ([element (worklist-first worklist)])
        (if element
            (cons element (f (element-next element)))
            '()))))

  (define/who element-remove!
    (lambda (element)
      (unless (element? element)
        (assertion-violation who "not an element" element))
      (let ([worklist (element-worklist element)])
        (element-worklist-set! element #f)
        (cond [(not worklist) (values)]
              [(element-previous element)
               => (lambda (previous)
                    (let ([next (element-next element)])
                      (element-next-set! previous next)
                      (element-previous-set! element #f)
                      (when next
                        (element-previous-set! next previous))))]
              [(element-next element)
               => (lambda (next)
                    (worklist-first-set! worklist next)
                    (element-previous-set! next #f))]
              [else
               (worklist-first-set! worklist #f)])
        (element-next-set! element #f))))

  ;; Record writers

  (record-writer (record-type-descriptor worklist)
    (lambda (r p wr)
      (put-string p "#<worklist ")
      (wr
       (let f ([element (worklist-first r)])
         (if element
             (cons element (f (element-next element)))
             '()))
       p)
      (put-string p ">")))

  (record-writer (record-type-descriptor element)
    (lambda (r p wr)
      (put-string p "#<element>")))

  )
