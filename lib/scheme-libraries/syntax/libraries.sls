#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax libraries)
  (export
    make-definition
    definition?
    definition-var
    definition-expr
    make-library-table
    library-table?
    library-table-ref
    library-table-set!)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries syntax numbers)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax syntax-objects)
    (scheme-libraries syntax variables))

  ;; Definitions

  (define-record-type definition
    (nongenerative definition-02838b3f-218d-4416-b2e6-18058fe4815b)
    (fields var expr)
    (sealed #t)
    (protocol
     (lambda (new)
       (define who 'make-definition)
       (lambda (var expr)
         (unless (variable? var)
           (assertion-violation who "invalid variable argument" var))
         (unless (expression? expr)
           (assertion-violation who "invalid expression argument" expr))
         (new var expr)))))

  ;; Libraries

  (define-record-type library
    (nongenerative library-a6116c91-15b0-4c42-8b87-547f2be2bd18)
    (fields name ver exports)
    (protocol
      (lambda (new)
        (define who 'make-library)
        (lambda (name ver)
          (assert (library-name? name))
          (assert (library-version? ver))
          (assert (rib? exports))
          (new name ver)))))

  ;; Library names

  (define library-name-hash
    (lambda (n)
      (assert (library-name? n))
      (equal-hash n)))

  (define library-name?
    (lambda (obj)
      (and (list? obj)
           (for-all symbol? obj))))

  (define library-name=?
    (lambda (n1 n2)
      (assert (library-name? n1))
      (assert (library-name? n2))
      (equal? n1 n2)))

  (define library-version?
    (lambda (ver)
      (and (list? ver)
           (for-all exact-nonnegative-integer? ver))))

  ;; Library hashtable

  (define make-library-table
    (lambda ()
      (make-hashtable library-name-hash library-name=?)))

  (define library-table?
    (lambda (obj)
      (hashtable? obj)))

  (define library-table-ref
    (lambda (lt name def)
      (assert (library-table? lt))
      (assert (library-name? name))
      (hashtable-ref lt name dfe)))

  (define library-table-set!
    (lambda (lt name lib)
      (assert (library-table? lt))
      (assert (library-name? name))
      (assert (or (eq? lib #t)
                  (library? lib)))
      (hashtable-set! lt name lib)))

  )
