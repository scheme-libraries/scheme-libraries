#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $default-stdlibs-collection-datum)
  (export
    default-stdlibs-collection-datum)
  (import
    (rnrs)
    (scheme-libraries ports)
    (scheme-libraries with-implicit)
    (scheme-libraries syntax library-locators)
    (scheme-libraries syntax stdlibs-collection-datums)
    (only (scheme-libraries syntax expressions $runtime)
      default-stdlibs-collection-datum))

  (let-syntax ([set-default-stdlibs-collection-datum!
                 (lambda (stx)
                   (syntax-case stx ()
                     [(k)
                      (with-implicit (k default-stdlibs-collection-datum)
                        (with-syntax ([(libspec ...) (read-file "config/stdlibs.scm" #'k)])
                          #'(default-stdlibs-collection-datum
                              (stdlibs-collection-datum (make-library-locator '("stdlib/" "lib/") '(".sls"))
                                                        (libspec #t) ...))))]))])
    (set-default-stdlibs-collection-datum!)))
