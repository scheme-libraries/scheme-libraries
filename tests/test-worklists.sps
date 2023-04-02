#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(import (rnrs)
        (scheme-libraries testing)
	(scheme-libraries worklists)
        (scheme-libraries record-writer))

(define-record-type entry
  (parent element)
  (sealed #t)
  (fields value)
  (protocol
   (lambda (pargs->new)
     (lambda (val)
       ((pargs->new) val)))))

(record-writer (record-type-descriptor entry)
  (lambda (r p wr)
    (put-string p "#<entry ")
    (wr (entry-value r) p)
    (put-string p ">")))

(test-begin "worklists")

(define entry1 (make-entry 1))
(define entry2 (make-entry 2))

(test-assert (entry? entry1))
(test-assert (entry? entry2))

(define worklist1 (make-worklist))
(define worklist2 (make-worklist))

(test-assert (worklist? worklist1))
(test-assert (worklist? worklist2))

(worklist-add! worklist1 entry1)
(test-eq worklist1 (element-worklist entry1))
(worklist-add! worklist1 entry2)
(test-eq worklist1 (element-worklist entry2))

(define accumulate
  (lambda (worklist)
    (let ([sum 0])
      (worklist-for-each
       (lambda (entry)
         (set! sum (+ sum (entry-value entry))))
       worklist)
      sum)))

(test-eqv 3 (accumulate worklist1))
(test-eqv 0 (accumulate worklist2))

(element-remove! entry1)
(test-eqv 2 (accumulate worklist1))
(worklist-add! worklist2 entry2)
(test-eqv 2 (accumulate worklist2))
(test-eqv 0 (accumulate worklist1))

(worklist-add! worklist1 entry1)
(worklist-add! worklist1 entry2)
(element-remove! entry2)
(test-eqv 1 (accumulate worklist1))
(element-remove! entry1)
(test-eqv 0 (accumulate worklist1))

(define worklist3 (make-worklist))
(define entry3 (make-entry 3))
(worklist-add! worklist3 entry1)
(worklist-add! worklist3 entry2)
(worklist-add! worklist3 entry3)

(test-equal '(3 2 1) (map entry-value (worklist->list worklist3)))

(define worklist4 (make-worklist))

(worklist-for-each
 (lambda (entry)
   (worklist-add! worklist4 entry))
 worklist3)

(test-assert (worklist-empty? worklist3))
(test-equal '(1 2 3) (map entry-value (worklist->list worklist4)))

(element-remove! entry2)
(test-equal '(1 3) (map entry-value (worklist->list worklist4)))

(test-assert (not (element-worklist entry2)))

(worklist-add! worklist4 entry2)

(test-equal '(2 1 3) (map entry-value (worklist->list worklist4)))

(test-end "worklists")
