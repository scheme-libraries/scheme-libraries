#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries ports)
  (export
    read-file
    textual-input-port?
    textual-output-port?)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries filenames))

  (define textual-input-port?
    (lambda (obj)
      (and (textual-port? obj)
           (input-port? obj))))

  (define textual-output-port?
    (lambda (obj)
      (and (textual-port? obj)
           (output-port? obj))))

  (define/who read-file
    (lambda (filename k)
      (unless (filename? filename)
        (assertion-violation who "invalid filename argument" filename))
      (unless (identifier? k)
        (assertion-violation who "invalid template identifier argument" k))
      (let ([p (open-file-input-port filename
                                     (file-options)
                                     (buffer-mode block)
                                     (native-transcoder))])
        (let f ([x (get-datum p)])
          (cond
           [(eof-object? x)
            (close-port p)
            '()]
           [else
            (cons (datum->syntax k x)
                  (f (get-datum p)))]))))))
