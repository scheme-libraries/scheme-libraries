#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax library-collections)
  (export
    make-library-collection
    library-collection?
    current-library-collection
    library-set!
    library-ref
    library-pending?
    library-pending!
    library-list-append!
    library-list
    uid->library
    object-set-symbol!
    object->symbol
    symbol->object
    mark->datum
    datum->mark
    location->datum
    datum->location
    variable->datum
    datum->variable)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries parameters)
    (scheme-libraries thread-parameters)
    (scheme-libraries define-who)
    (scheme-libraries hashtables)
    (scheme-libraries uuid)
    (scheme-libraries syntax $environments)
    (scheme-libraries syntax $marks)
    (scheme-libraries syntax variables)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects))

  ;; Library collections

  (define-record-type library-collection
    (nongenerative library-collection-765810f4-88f2-47a3-9d4a-df90941f0a82)
    (sealed #t)
    (fields uid-table libraries pending object-table symbol-table (mutable list))
    (protocol
      (lambda (new)
        (lambda ()
          (new (make-eq-hashtable)
               (make-library-table)
               (make-library-table)
               (make-eq-hashtable)
               (make-eq-hashtable)
               '())))))

  ;; Current library collection

  (define/who current-library-collection
    (make-thread-parameter (make-library-collection)
      (lambda (x)
        (unless (library-collection? x)
          (assertion-violation who "invalid library collection" x))
        x)))

  (define current-library-table
    (lambda ()
      (library-collection-libraries (current-library-collection))))

  (define current-pending-table
    (lambda ()
      (library-collection-pending (current-library-collection))))

  (define library-set!
    (lambda (name lib)
      (library-table-set! (current-library-table) name lib)
      (hashtable-update! (library-collection-uid-table (current-library-collection))
                         (library-uid lib)
                         (lambda (old-val)
                           (when old-val (assert #f))
                           lib)
                         #f)))

  (define library-ref
    (lambda (name default)
      (library-table-ref (current-library-table) name default)))

  (define uid->library
    (lambda (uid)
      (assert (symbol? uid))
      (assert (hashtable-ref (library-collection-uid-table (current-library-collection))
                             uid
                             #f))))

  (define library-pending?
    (lambda (name)
      (library-table-contains? (current-pending-table) name)))

  (define library-pending!
    (lambda (name flag)
      (let ([tbl (current-pending-table)])
        (if flag
            (library-table-set! tbl name #t)
            (library-table-delete! tbl name)))))

  (define library-list-append!
    (lambda (lib)
      (let ([coll (current-library-collection)])
        (library-collection-list-set! coll
                                      (cons lib (library-collection-list coll))))))

  (define library-list
    (lambda ()
      (reverse (library-collection-list (current-library-collection)))))

  ;; Serialization

  ;; XXX: Is this used?
  (define object-set-symbol!
    (lambda (obj sym)
      (hashtable-set! (library-collection-object-table (current-library-collection)) obj sym)
      (hashtable-set! (library-collection-symbol-table (current-library-collection)) sym obj)))

  (define object->symbol
    (lambda (type obj)
      (assert (symbol? type))
      (hashtable-intern! (library-collection-object-table (current-library-collection))
                         obj
                         (lambda ()
                           (let ([sym (uid type)])
                             (hashtable-set! (library-collection-symbol-table (current-library-collection)) sym obj)
                             sym)))))

  (define symbol->object
    (lambda (sym fail)
      (assert (symbol? sym))
      (assert (procedure? fail))
      (hashtable-intern! (library-collection-symbol-table (current-library-collection))
                         sym
                         fail)))

  ;; Serializers

  (define/who mark->datum
    (lambda (m)
      (assert (mark? m))
      ;; XXX: We might be able to output an anti-mark.  Check this.
      (assert (not (anti-mark? m)))
      (mark-name m)))

  (define/who datum->mark
    (lambda (s)
      (assert (symbol? s))
      (symbol->object s (lambda () (make-mark s)))))

  (define/who location->datum
    (lambda (loc)
      (assert (location? loc))
      (location-name loc)))

  (define/who datum->location
    (lambda (s)
      (assert (symbol? s))
      (symbol->object s (lambda () (make-location s)))))

  (define variable->datum
    (lambda (var)
      (assert (variable? var))
      (string->symbol (format "%variable-~a" (variable-name var)))))

  (define datum->variable
    (lambda (s)
      (assert (symbol? s))
      (let ([name
             (let ([s (symbol->string s)])
               (string->symbol
                (substring s
                           (string-length "%variable-")
                           (fx- (string-length s)
                                (string-length "%variable-")))))])
        (symbol->object s (lambda () (name->variable name)))))))
