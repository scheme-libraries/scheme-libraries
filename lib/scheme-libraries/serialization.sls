#!r6rs

(library (scheme-libraries serialization)
  (export
    serialize
    deserialize)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries helpers)
    (scheme-libraries lists)
    (scheme-libraries numbers))

  (define-syntax empty-symbol
    (lambda (stx)
      (syntax-quote (string->symbol ""))))

  (define serializers (make-eqv-hashtable))

  (define/who record-serializer
    (lambda (rtd proc)
      (unless (record-type-descriptor? rtd)
        (assertion-violation who "invalid record type descriptor argument" rtd))
      (unless (procedure? proc)
        (assertion-violation who "invalid procedure argument" proc))
      (hashtable-update! serializers
        rtd
        (lambda (old-val)
          (when old-val
            (assertion-violation who "record type already registered" rtd))
          proc)
        #f)))

  (define/who serialize
    (lambda (x)
      (define-record-type hole
        (nongenerative hole-b5635d27-1ee6-45d7-9418-3430c8682cd5)
        (sealed #t) (opaque #t)
        (fields (mutable serialized) (mutable index)))
      (define ht (make-eq-hashtable))
      (define count 0)
      (define e
        (let f ([x x])
          (cond
           [(or (boolean? x)
                (number? x)
                (char? x)
                (null? x))
            x]
           [(symbol? x)
            (if (eq? x empty-symbol)
                `(empty-symbol)
                x)]
           [(hashtable-ref ht x #f)
            => (lambda (hole)
                 (hole-index-set! hole #t)
                 hole)]
           [else
            (let ([hole (make-hole #f #f)])
              (hashtable-set! ht x hole)
              (let ([e (do-serialize x f)])
                (hole-serialized-set! hole e)
                hole))])))
      (let f ([e e])
        (cond
         [(hole? e)
          (let ([idx (hole-index e)])
            (cond
             [(exact-integer? idx)
              `(ref ,idx)]
             [idx
              (let ([idx count])
                (set! count (+ 1 count))
                (hole-index-set! e idx)
                `(mark ,idx ,(f (hole-serialized e))))]
             [else
              (f (hole-serialized e))]))]
         [(pair? e)
          (cons (f (car e)) (f (cdr e)))]
         [(vector? e)
          (vector-map f e)]
         [else e]))))

  (define do-serialize
    (lambda (x f)
      (cond
       [(list? x)
        `(list ,@(map f x))]
       [(pair? x)
        (if (length+ x)
            `(cons* ,@(let g ([x x])
                        (if (pair? x)
                            (cons (g (car x))
                                  (f (cdr x)))
                            (f x))))
            `(cons ,(f (car x)) (f (cdr x))))]
       [(vector? x)
        `(vector ,@(vector->list (vector-map f x)))]
       [(string? x) x]
       [(bytevector? x) x]
       [(/record/
         -> check record, etc.
         )]
       [else
        (assert #f)])))

  (define deserialize
    (lambda (e)
      (assert #f))))
