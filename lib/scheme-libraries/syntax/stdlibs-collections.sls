#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax stdlibs-collections)
  (export
    stdlibs-collection)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries syntax $make-stdlibs-collection-expr)
    (scheme-libraries syntax $parsers)
    (scheme-libraries syntax syntax-match))

  ;; A library in a stdlibs collection is identified by its name,
  ;; which is also used by the library locator to retrieve its source
  ;; code.

  ;; All system libraries are compiled together and are later invoked
  ;; together.  A library, on which a system library depends must also
  ;; be a system library.  It is an error if this is not fulfilled.

  #;
  (standard-library-collection
    library-locator
    ((rnrs) #f #f)
    ((...) #f #t)
    ((...) ...)
    (<lib> <system?> <visible?>)
    ...)

  ;; Helpers

  (define-syntax metalet
    (lambda (x)
      (syntax-case x ()
        [(_ ([var expr] ...) body1 ... body2)
         (for-all identifier? #'(var ...))
         #'#'(let-syntax ([m (lambda (x)
                               (let ([var expr] ...)
                                 body1 ... body2))])
               m)])))

  ;; Syntax

  (define-syntax/who stdlibs-collection
    (lambda (x)
      (syntax-case x ()
        [(_ loc-expr [lib-ref sys?] ...)
         (for-all boolean? (map syntax->datum #'(sys? ... )))
         (metalet ([loc loc-expr]
                   [lib-ref* (list (syntax-extend-backquote here `lib-ref) ...)]
                   [sys?* (list sys? ...)])
           (let ([stdlib*
                  (map (lambda (lib-ref-expr sys?-expr)
                         (let-values ([(name pred)
                                       (parse-library-reference lib-ref-expr)])
                           (make-stdlib name pred (syntax->datum sys?-expr) )))
                       lib-ref* sys?*)])
             (make-stdlibs-collection-expr loc stdlib*)))]
        [_ (syntax-violation who "invalid syntax" x)])))




  )
