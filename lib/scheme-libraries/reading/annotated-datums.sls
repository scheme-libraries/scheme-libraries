#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries reading annotated-datums)
  (export
    datum->annotated-datum
    annotated-datum?
    annotated-datum-source-location
    annotated-datum-value
    make-annotated-atom
    annotated-atom?
    make-annotated-pair
    annotated-pair?
    annotated-pair-car
    annotated-pair-cdr
    make-annotated-list
    make-annotated-dotted-list
    make-annotated-vector
    annotated-vector?
    annotated-vector-ref
    annotated-vector->list
    invalid-datum-condition)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries atoms)
    (scheme-libraries numbers)
    (scheme-libraries reading source-locations))

  (define-record-type annotated-datum
    (nongenerative annotated-datum-21b92024-d6af-4086-b8c6-4b9cd9017d01)
    (sealed #t)
    (fields
      expression
      source-location
      value))

  (define/who datum->annotated-datum
    (lambda (x)
      (let ([seen (make-eq-hashtable)])
        (let f ([x x])
          (cond
           [(hashtable-ref seen x #f)
            (invalid-datum-condition)]
           [(pair? x)
            (hashtable-set! seen x #t)
            (make-annotated-pair (f (car x)) (f (cdr x)) #f)]
           [(vector? x)
            (hashtable-set! seen x #t)
            (make-annotated-vector (map f (vector->list x)) #f)]
           [(atom? x)
            (make-annotated-atom x #f)]
           [else
            (invalid-datum-condition)])))))

  (define/who make-annotated-atom
    (lambda (value source-location)
      (unless (atom? value)
        (assertion-violation who "invalid value argument" value))
      (unless
          (or (not source-location)
              (source-location? source-location))
        (assertion-violation who "invalid source-location argument" source-location))
      (make-annotated-datum value source-location value)))

  (define/who make-annotated-pair
    (lambda (car cdr source-location)
      (unless (annotated-datum? car)
        (assertion-violation who "invalid car argument" car))
      (unless (annotated-datum? cdr)
        (assertion-violation who "invalid cdr argument" cdr))
      (unless
          (or (not source-location)
              (source-location? source-location))
        (assertion-violation who "invalid source-location argument" source-location))
      (make-annotated-datum (cons car cdr)
                            source-location
                            (cons (annotated-datum-value car)
                                  (annotated-datum-value cdr)))))

  (define/who make-annotated-list
     (lambda (element* source-location)
       (unless (and (list? element*)
                    (for-all annotated-datum? element*))
         (assertion-violation who "invalid element list argument" element*))
       (unless
           (or (not source-location)
               (source-location? source-location))
         (assertion-violation who "invalid source-location argument" source-location))
       (fold-right
        (lambda (element annotated-list)
          (make-annotated-datum
           (cons element
                 annotated-list)
           source-location
           (cons (annotated-datum-value element)
                 (annotated-datum-value annotated-list))))
        (make-annotated-atom '() source-location)
        element*)))

  (define/who make-annotated-dotted-list
     (lambda (element* element source-location)
       (unless (and (list? element*)
                    (for-all annotated-datum? element*))
         (assertion-violation who "invalid element list argument" element*))
       (unless (annotated-datum? element)
         (assertion-violation who "invalid element argument" element))
       (unless
           (or (not source-location)
               (source-location? source-location))
         (assertion-violation who "invalid source-location argument" source-location))
       (fold-right
        (lambda (element annotated-list)
          (make-annotated-datum
           (cons element
                 annotated-list)
           source-location
           (cons (annotated-datum-value element)
                 (annotated-datum-value annotated-list))))
        element
        element*)))

  (define/who make-annotated-vector
    (lambda (element* source-location)
      (define element-vector
        (begin
          (unless (and (list? element*)
                       (for-all annotated-datum? element*))
            (assertion-violation who "invalid element list argument" element*))
          (list->vector element*)))
      (unless
          (or (not source-location)
              (source-location? source-location))
        (assertion-violation who "invalid source-location argument" source-location))
      (make-annotated-datum
       element-vector
       source-location
       (vector-map annotated-datum-value element-vector))))

  (define/who annotated-atom?
    (lambda (obj)
      (atom? (unwrap obj))))

  (define/who annotated-pair?
    (lambda (obj)
      (pair? (unwrap obj))))

  (define/who annotated-vector?
    (lambda (obj)
      (vector? (unwrap obj))))

  (define/who annotated-pair-car
    (lambda (annotation)
      (cond
       [(pair? annotation)
        (car annotation)]
       [(and (annotated-datum? annotation)
             (pair? (annotated-datum-expression annotation)))
        (wrap (car (annotated-datum-expression annotation)) annotation)]
       [else
        (assertion-violation who "invalid annotation argument" annotation)])))

  (define/who annotated-pair-cdr
    (lambda (annotation)
      (cond
       [(pair? annotation)
        (cdr annotation)]
       [(and (annotated-datum? annotation)
             (pair? (annotated-datum-expression annotation)))
        (wrap (cdr (annotated-datum-expression annotation)) annotation)]
       [else
        (assertion-violation who "invalid annotation argument" annotation)])))

  (define/who annotated-vector-ref
    (lambda (annotation index)
      (unless (exact-nonnegative-integer? index)
        (assertion-violation who "invalid index argument" annotation))
      (cond
       [(vector? annotation)
        (vector-ref annotation index)]
       [(and (annotated-datum? annotation)
             (vector? (annotated-datum-expression annotation)))
        (wrap (vector-ref index (annotated-datum-expression annotation)) annotation)]
       [else
        (assertion-violation who "invalid annotation argument" annotation)])))

  (define/who annotated-vector->list
    (lambda (ann)
      (cond
       [(vector? ann)
        (vector->list ann)]
       [(annotated-vector? ann)
        (make-annotated-list (vector->list (annotated-datum-expression ann))
                             (annotated-datum-source-location ann))]
       [else
        (assertion-violation who "invalid annotation argument" ann)])))

  (define unwrap
    (lambda (obj)
      (if (annotated-datum? obj)
          (annotated-datum-expression obj)
          obj)))

  (define wrap
    (lambda (inner outer)
      (cond
       [(annotated-datum-source-location inner) inner]
       [(annotated-datum-source-location outer)
        => (lambda (source-location)
             (make-annotated-datum
              (annotated-datum-expression inner)
              source-location
              (annotated-datum-value inner)))]
       [else inner])))

  ;; Conditions

  (define invalid-datum-condition
    (lambda ()
      (raise (make-invalid-datum-condition))))

  (define-condition-type &invalid-datum
    &condition
    make-invalid-datum-condition invalid-datum-condition?)

  )
