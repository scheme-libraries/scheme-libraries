#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $labels)
  (export
    make-label
    label?
    label=?
    label-metalevel-set!
    label-bind!
    label-kill!
    label->binding
    label-binding-set!
    make-label/props
    label/props?
    label/props-merge
    label/props-label)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries lists)
    (scheme-libraries rec)
    (scheme-libraries gensyms)
    (scheme-libraries hashtables)
    (scheme-libraries syntax $metalevels)
    (scheme-libraries syntax $syntax-types))

  ;; Label

  (define-record-type label
    (nongenerative label-a39e36ea-e0e4-4a27-9274-b982710361d8)
    (sealed #t)
    (fields
      (mutable binding)
      (mutable metalevel)
      (mutable name))
    (protocol
      (lambda (new)
        (define who 'make-label)
        (rec make
          (case-lambda
            [()
             (make #f (current-metalevel) #f)]
            [(bdg)
             (make bdg (current-metalevel) #f)]
            [(bdg ml)
             (make bdg ml #f)]
            [(bdg ml name)
             (let ([bdg (or bdg (make-displaced-binding))])
               (unless (binding? bdg)
                 (assertion-violation who "invalid label argument" bdg))
               (unless (metalevel? ml)
                 (assertion-violation who "invalid metalevel argument" ml))
               (unless (or (not name)
                           (symbol? name))
                 (assertion-violation who "invalid symbol argument" name))
               (new bdg ml name))])))))

  (define/who label=?
    (lambda (l1 l2)
      (unless (label? l1)
        (assertion-violation who "invalid first label argument" l1))
      (unless (label? l2)
        (assertion-violation who "invalid second label argument" l2))
      (eq? l1 l2)))

  (define/who label-bind!
    (lambda (lbl bdg)
      (unless (label? lbl)
        (assertion-violation who "invalid label argument" lbl))
      (unless (binding? bdg)
        (assertion-violation who "invalid binding argument" bdg))
      (label-binding-set! lbl bdg)))

  (define/who label-kill!
    (lambda (lbl)
      (unless (label? lbl)
        (assertion-violation who "invalid label argument" lbl))
      (label-binding-set! lbl (make-displaced-binding))))

  (define/who label->binding
    (lambda (lbl)
      (unless (or (not lbl) (label? lbl))
        (assertion-violation who "invalid labels argument" lbl))
      (and lbl
	   (let ([bdg (label-binding lbl)])
             (if (in-phase? (label-metalevel lbl)
                            (current-metalevel))
                 bdg
                 (make-out-of-phase-binding))))))

  (define in-phase?
    (lambda (ml cml)
      (assert (metalevel? ml))
      (assert (metalevel? cml))
      (or (fx=? ml cml)
	  (and (fxnegative? ml)
	       (fx<=? (fxnot ml) cml)))))

  (define label->datum
    (let ([ht (make-eq-hashtable)])
      (lambda (lbl)
        (assert (label? lbl))
        (intern! ht
                 lbl
                 (lambda ()
                   ;; PROBLEM: The label needs to be recorded somewhere. Otherwise we place the same label several times.
                   (list (binding->datum (label-binding lbl))
                         (label-metalevel lbl)))))))

  (define datum->label
    (let ([ht (make-eq-hashtable)])
      (lambda (s)
        (assert ))))

  ;; Labels with props

  (define-record-type label/props
    (nongenerative label/props-b0f0a45a-a705-448c-b96f-ef8655d2b2b7)
    (sealed #t)
    (fields label props)
    (protocol
      (lambda (new)
        (define who 'make-label/props)
        (rec make
          (case-lambda
            [(lbl) (make lbl '())]
            [(lbl props)
             (unless (label? lbl)
               (assertion-violation who "invalid labels argument" lbl))
             (unless (and (list? props)
                          (for-all label? props))
               (assertion-violation who "invalid property list argument" props))
             (new lbl props)])))))

  (define/who label/props-merge
    (lambda (new-l/p prev-l/p)
      (define lbl
        (begin
          (unless (label/props? new-l/p)
            (assertion-violation who "invalid new label/props argument" new-l/p))
          (unless (label/props? prev-l/p)
            (assertion-violation who "invalid previous label/props argument" prev-l/p))
          (label/props-label new-l/p)))
      (if (label=? lbl (label/props-label prev-l/p))
	  (make-label/props lbl
			    (delete-duplicates
			     (append (label/props-props new-l/p)
				     (label/props-props prev-l/p))
                             label=?))
	  new-l/p)))
  )
