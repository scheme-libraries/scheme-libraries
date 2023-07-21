#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax libraries)
  (export
    make-definition
    definition?
    definition-var
    definition-expr
    make-library
    library?
    library-name
    library-version
    library-exports
    library-name=?
    library-visit!
    library-invoke!
    make-library-table
    library-table?
    library-table-ref
    library-table-set!
    library-table-delete!
    library-table-contains?
    make-requirements-collector
    requirements-collector?
    with-requirements-collector
    require-for-runtime!
    require-for-expand!
    current-runtime-globals
    collected-visit-requirements
    collected-invoke-requirements)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries boxes)
    (scheme-libraries define-who)
    (scheme-libraries numbers)
    (scheme-libraries parameters)
    (scheme-libraries syntax expressions)
    (scheme-libraries syntax $ribs)
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
    (fields
      ;; The library name as a list of symbols.
      name
      ;; The version as a list of nonnegative exact numbers.
      version
      ;; The exported identifiers as a rib.
      exports
      ;; The visit requirements as a vector of libraries.
      visit-requirements
      ;; The invoke requirements as a vector of libraries.
      invoke-requirements
      ;; The visit procedure of #t if visiting or #f if visited.
      (mutable visiter)
      ;; The invoke procedure or #t if invoking or #f if invoked.
      (mutable invoker))
    (protocol
      (lambda (new)
        (define who 'make-library)
        (lambda (name ver exports visreqs invreqs visiter invoker)
          (assert (library-name? name))
          (assert (library-version? ver))
          (assert (rib? exports))
          (assert (and (vector? visreqs)
                       (for-all library? (vector->list visreqs))))
          (assert (and (vector? invreqs)
                       (for-all library? (vector->list invreqs))))
          (assert (or (not visiter) (procedure? visiter)))
          (assert (or (not invoker) (procedure? invoker)))
          (new name ver exports visreqs invreqs visiter invoker)))))

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

  (define library-table-contains?
    (lambda (table name)
      (assert (library-name? name))
      (hashtable-contains? table name)))

  (define library-table-delete!
    (lambda (table name)
      (assert (library-name? name))
      (hashtable-delete! table name)))

  (define library-visit!
    (lambda (lib)
      (put-string (current-error-port) "FIXME: Implement library-visit!\n")))

  (define library-invoke!
    (lambda (lib)
      (assert (library? lib))
      (let ([name (library-name lib)]
            [invoker (library-invoker lib)])
        (when invoker
          (dynamic-wind
            (lambda ()
              (library-invoker-set! lib #t))
            (lambda ()
              (vector-for-each library-invoke! (library-invoke-requirements lib)))
            (lambda ()
              (library-invoker-set! lib invoker)))
          (library-invoker-set! lib
                                (lambda ()
                                  (assertion-violation #f (format "invocation of library ~a did not return" name))))
          (when (eq? invoker #t)
            (assertion-violation #f (format "circular invocation of library ~a" name)))
          (invoker)
          (library-invoker-set! lib #f)))))

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
      (hashtable-ref lt name def)))

  (define library-table-set!
    (lambda (lt name lib)
      (assert (library-table? lt))
      (assert (library-name? name))
      (assert (or (eq? lib #t)
                  (library? lib)))
      (hashtable-set! lt name lib)))

  ;; Invoke and visit requirements

  (define-record-type global
    (nongenerative global-97fdc653-e31c-4ef5-a444-b626a5f823aa)
    (sealed #t)
    (fields library location)
    (protocol
      (lambda (new)
        (lambda (lib loc)
          (assert (library? lib))
          (assert (box? loc))
          (new lib loc)))))

  (define-syntax/who with-requirements-collector
    (lambda (stx)
      (syntax-case stx ()
        [(_ b1 ... b2)
         #'(parameterize ([current-requirements-collector (make-requirements-collector)])
             b1 ... b2)]
        [_ (syntax-violation who "invalid syntax" stx)])))

  (define-record-type requirements-collector
    (nongenerative requirements-collector-331810a0-1fb7-48de-a422-aaf88b667810)
    (fields invokes visits runtime-globals expand-globals)
    (protocol
      (lambda (new)
        (lambda ()
          (new (make-eq-hashtable)
               (make-eq-hashtable)
               (make-eq-hashtable)
               (make-eq-hashtable))))))

  (define/who current-requirements-collector
    (make-parameter #f
      (lambda (x)
        (unless (or (not x) (requirements-collector? x))
          (assertion-violation who "invalid requirements collector argument" x))
        x)))

  (define current-visit-requirements
    (lambda ()
      (requirements-collector-visits (assert (current-requirements-collector)))))

  (define current-invoke-requirements
    (lambda ()
      (requirements-collector-invokes (assert (current-requirements-collector)))))

  (define current-runtime-requirements
    (lambda ()
      (requirements-collector-runtime-globals (assert (current-requirements-collector)))))

  ;; Do we need this?
  (define current-expand-requirements
    (lambda ()
      (requirements-collector-expand-globals (assert (current-requirements-collector)))))

  (define require-for-runtime!
    (lambda (lib var loc)
      (assert (library? lib))
      (assert (variable? var))
      (assert (box? loc))
      (hashtable-set! (current-invoke-requirements) lib #t)
      (hashtable-set! (current-runtime-requirements) var (make-global lib loc))))

  (define require-for-expand!
    (lambda (lib var loc)
      (assert (library? lib))
      (assert (variable? var))
      (assert (box? loc))
      (hashtable-set! (current-visit-requirements) lib #t)
      (hashtable-set! (current-expand-requirements) var (make-global lib loc))))

  (define current-runtime-globals
    (lambda ()
      (let-values ([(vars globals) (hashtable-entries (current-runtime-requirements))])
        (values vars (vector-map global-library globals) (vector-map global-location globals)))))

  (define collected-visit-requirements
    (lambda ()
      (hashtable-keys (current-visit-requirements))))

  (define collected-invoke-requirements
    (lambda ()
      (hashtable-keys (current-invoke-requirements))))

  )
