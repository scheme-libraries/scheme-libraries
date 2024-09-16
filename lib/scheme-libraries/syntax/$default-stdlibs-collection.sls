#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $default-stdlibs-collection)
  (export
    default-stdlibs-collection)
  (import
    (rnrs)
    (scheme-libraries ports)
    (scheme-libraries syntax library-locators)
    (scheme-libraries syntax stdlibs-collection-datums)
    (scheme-libraries syntax $serializing)
    (prefix (scheme-libraries syntax expressions $runtime)
      $))

  (define default-stdlibs-collection
    (lambda ()
      ($default-stdlibs-collection)))

  (let-syntax ([set-default-stdlibs-collection!
                (lambda (stx)
                  (syntax-case stx ()
                    [(k)
                     (with-syntax ([(libspec ...) (read-file "config/stdlibs.scm" #'k)])
                       #'(begin
                           ($default-stdlibs-collection
                            (datum->library-collection
                             stdlibs-collection-datum (make-library-locator '("stdlib/" "lib/") '(".sls"))
                             (libspec #t) ...))))]))])
    (set-default-stdlibs-collection!)))
