#!r6rs

(library (scheme-libraries assembly-output $common)
  (export
    assembly-output-port)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries ports)
    (scheme-libraries thread-parameters))

  (define/who assembly-output-port
    (make-thread-parameter
     (current-output-port)
     (lambda (port)
       (unless (textual-output-port? port)
         (assertion-violation who "invalid port argument" port))
       port))))
