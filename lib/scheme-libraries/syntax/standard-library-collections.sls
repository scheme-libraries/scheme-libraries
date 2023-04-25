#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax standard-library-collections)
  (export
    standard-library-collection)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries syntax $parsers)
    (scheme-libraries syntax $make-stdlibs-collection-expr))

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

  (define-syntax/who stdlibs-collection
    (lambda (x)
      (syntax-case x ()
        [(_ loc-expr [lib-ref sys? vis?] ...)
         (for-all boolean? (map syntax->datum #'(sys? ... vis? ...) ))
         (let ([stdlib*
                (map (lambda (libref sys? vis?)
                       (let-values ([(name pred)
                                     (parse-library-reference libref)])
                         (make-stdlib name pred (syntax->datum sys?) (syntax->datum vis?))))
                     #'(libref ...) #'(sys? ...) #'(vis? ...))])
           (metalet ([loc loc-expr])
             (make-stdlibs-collection-expr loc stdlib*)))]
        [_ (syntax-violation who "invalid syntax" x)])))

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


  )
