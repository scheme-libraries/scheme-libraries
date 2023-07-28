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
    object->symbol
    symbol->object)
  (import
    (rnrs)
    (scheme-libraries parameters)
    (scheme-libraries thread-parameters)
    (scheme-libraries define-who)
    (scheme-libraries hashtables)
    (scheme-libraries uuid)
    (scheme-libraries syntax $environments)
    (scheme-libraries syntax libraries)
    (scheme-libraries syntax syntax-match)
    (scheme-libraries syntax syntax-objects))

  ;; Library collections

  (define-record-type library-collection
    (nongenerative library-collection-765810f4-88f2-47a3-9d4a-df90941f0a82)
    (sealed #t)
    (fields libraries pending object-table symbol-table (mutable list))
    (protocol
      (lambda (new)
        (lambda ()
          (new (make-library-table)
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
      (library-table-set! (current-library-table) name lib)))

  (define library-ref
    (lambda (name default)
      (library-table-ref (current-library-table) name default)))

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

  )
