#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax default-stdlibs-collections)
  (export
    make-default-stdlibs-collection)
  (import
    (rnrs)
    (scheme-libraries syntax $default-stdlibs-collection-datum)
    (scheme-libraries syntax $serializing))

  (define make-default-stdlibs-collection
    (lambda ()
      (datum->library-collection (default-stdlibs-collection-datum)))))
