#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library ($record-names)
  (export
    define-record-name
    record-type-descriptor
    record-constructor-descriptor)
  (import
    ($system)
    (scheme-libraries define-who))

  (define-syntax/who define-record-name
    (lambda (x)
      (syntax-case x ()
        [(_ record-name rtd-id cd-id)
         (and (identifier? #'record-name)
              (identifier? #'rtd-id)
              (identifier? #'cd-id))
         #'(begin
             (define-syntax/who record-name
               (lambda (x)
                 (syntax-violation who "invalid record name syntax" x)))
             (define-property record-name record-type-descriptor-key #'rtd-id)
             (define-property record-name record-constructor-descriptor-key #'cd-id))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax/who record-type-descriptor
    (lambda (x)
      (syntax-case x ()
        [(_ name)
         (identifier? #'name)
         (lambda (lookup)
           (or (lookup #'name #'record-type-descriptor-key)
               (syntax-violation who "invalid record name syntax" x #'name)))]
        [_
         (syntax-violation who "invalid syntax" x)])))

  (define-syntax/who record-constructor-descriptor
    (lambda (x)
      (syntax-case x ()
        [(_ name)
         (identifier? #'name)
         (lambda (lookup)
           (or (lookup #'name #'record-constructor-descriptor-key)
               (syntax-violation who "invalid record name syntax" x #'name)))]
        [_ (syntax-violation who "invalid syntax" x)])))

  ;; Property keys
  (define record-type-descriptor-key)
  (define record-constructor-descriptor-key))
