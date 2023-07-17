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

  ;; need include path... need expand-time library locator.
  ;; how do we find it?  from default library-locator... we have to set it during load time...?
  ;;
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
        [(_ loc-expr [lib-ref sys? vis?] ...)
         (for-all boolean? (map syntax->datum #'(sys? ... vis? ...)))
         (metalet ([loc loc-expr]
                   [lib-ref* (list (syntax-extend-backquote here `lib-ref) ...)]
                   [sys?* (list sys? ...)]
                   [vis?* (list vis? ...)])
           (let ([stdlib*
                  (map (lambda (lib-ref-expr sys?-expr vis?-expr)
                         (let-values ([(name pred)
                                       (parse-library-reference lib-ref-expr)])
                           (make-stdlib name pred (syntax->datum sys?-expr) (syntax->datum vis?-expr))))
                       lib-ref* sys?* vis?*)])
             (make-stdlibs-collection-expr loc stdlib*)))]
        [_ (syntax-violation who "invalid syntax" x)])))




  )
