#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax standard-library-collections)
  (export
    standard-library-collection)
  (import
    (rnrs)
    (scheme-libraries define-who))

  ;; need include path... need expand-time library locator.
  ;; how do we find it?  from default library-locator... we have to set it during load time...?
  ;;
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
        [(_ loc-expr [(name1 ... name2) sys? vis?] ...)
         ;; TODO: expand-time checking of names and lib names, etc.
         #'(let-syntax ([instantiate
                         (let ([loc loc-expr])
                           (lambda (x)
                             (make-stdlibs-collection-expr loc '(((name1 ... name2) sys? vis?) ...))))])
             (instantiate))]
        [_ (syntax-violation who "invalid syntax" x)]))))
