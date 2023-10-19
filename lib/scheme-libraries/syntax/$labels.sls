#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $labels)
  (export
    make-label
    label?
    label-name
    label=?
    label-binding
    label-metalevel-set!
    label-bind!
    label-kill!
    label->binding
    label-binding-set!
    make-label/props
    label/props?
    label/props-merge
    label/props-label
    label/props-props
    label/props-ref
    make-property
    property?
    property-key-label
    property-value-label)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries lists)
    (scheme-libraries rec)
    (scheme-libraries uuid)
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
             (let ([bdg (or bdg (make-displaced-binding))]
                   [name (or name (uid '%label))])
               (unless (binding? bdg)
                 (assertion-violation who "invalid label argument" bdg))
               (unless (metalevel? ml)
                 (assertion-violation who "invalid metalevel argument" ml))
               (unless (symbol? name)
                 (assertion-violation who "invalid symbol argument" name))
               (new bdg ml name))])))))

  (define/who label=?
    (lambda (l1 l2)
      (unless (label? l1)
        (assertion-violation who "invalid first label argument" l1))
      (unless (label? l2)
        (assertion-violation who "invalid second label argument" l2))
      (eq? l1 l2)))

  (define make-label-table
    (lambda ()
      (make-eq-hashtable)))

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

  ;; Labels with props

  (define-record-type property
    (nongenerative property-8f3157b8-6e11-4e4f-8a06-030ee2b0d8ae)
    (fields key-label value-label)
    (protocol
      (lambda (new)
        (lambda (key-lbl val-lbl)
          (assert (label? key-lbl))
          (assert (label? val-lbl))
          (new key-lbl val-lbl)))))

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
            [(lbl prop*)
             (unless (label? lbl)
               (assertion-violation who "invalid labels argument" lbl))
             (unless (and (list? prop*)
                          (for-all property? prop*))
               (assertion-violation who "invalid property list argument" prop*))
             (new lbl prop*)])))))

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
          (let ([prev-props (label/props-props prev-l/p)])
            (if (null? prev-props)
                new-l/p
                (make-label/props lbl (append (label/props-props new-l/p)
                                              prev-props))))
          new-l/p)))

  (define/who label/props-ref
    (lambda (l/p key-lbl)
      (cond
       [(find (lambda (prop)
                (label=? (property-key-label prop)
                         key-lbl))
              (label/props-props l/p))
        => property-value-label]
       [else #f])))
  )
