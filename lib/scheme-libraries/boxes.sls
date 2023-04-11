#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries boxes)
  (export
    (rename (make-box box))
    box?
    unbox
    set-box!)
  (import
    (rnrs)
    (scheme-libraries record-writer))

  (define-record-type (box make-box box?)
    (nongenerative box-7da12061-a79b-4a61-b802-e215c66c6fee)
    (sealed #t)
    (fields (mutable contents unbox set-box!)))

  (record-writer (record-type-descriptor box)
    (lambda (r p wr)
      (put-string p "#<box ")
      (wr (unbox r) p)
      (put-string p ">")))
  )
