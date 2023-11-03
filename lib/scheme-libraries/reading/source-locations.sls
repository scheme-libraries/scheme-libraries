#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries reading source-locations)
  (export
    make-source-location
    source-location?
    source-location-filename
    source-location-start
    source-location-end
    &source-location-condition
    make-source-location-condition
    source-location-condition?
    condition-source-location
    format-source-location
    display-source-location
    source-location->s-exp
    s-exp->source-location)
  (import
    (rnrs)
    (scheme-libraries basic-format-strings)
    (scheme-libraries define-who)
    (scheme-libraries filenames)
    (scheme-libraries match)
    (scheme-libraries ports)
    (scheme-libraries reading positions)
    (scheme-libraries record-writer))

  (define-record-type source-location
    (nongenerative source-location-bb7980cc-5e74-40f2-ade8-ad2926faf389)
    (fields filename start end)
    (protocol
      (lambda (new)
        (define who 'make-source-location)
        (lambda (filename start end)
          (unless (or (not filename)
                      (filename? filename))
            (assertion-violation who "invalid filename argument" filename))
          (unless (position? start)
            (assertion-violation who "invalid start argument" start))
          (unless (position? end)
            (assertion-violation who "invalid end argument" end))
          (new filename start end)))))

  (define-condition-type &source-location-condition &condition
    make-source-location-condition
    source-location-condition?
    (source-location condition-source-location))

  (define/who display-source-location
    (case-lambda
      [(source-location)
       (display-source-location source-location (current-output-port))]
      [(source-location port)
       (unless (source-location? source-location)
         (assertion-violation who "invalid source location argument" source-location))
       (unless (textual-output-port? port)
         (assertion-violation who "invalid port argument" port))
       (display (format-source-location source-location) port)]))

  (define format-source-location
    (lambda (s)
      (format "~a: ~a-~a"
              (source-location-filename s)
              (format "~a.~a"
                      (position-line (source-location-start s))
                      (position-column (source-location-start s)))
              (if (=  (position-line (source-location-start s))
                      (position-line (source-location-end s)))
                  (format "~a" (position-column (source-location-end s)))
                  (format "~a.~a"
                          (position-line (source-location-end s))
                          (position-column (source-location-end s)))))))

  ;; Serializing

  (define/who source-location->s-exp
    (lambda (loc)
      (unless (source-location? loc)
        (assertion-violation who "invalid source location argument" loc))
      (let ([filename (source-location-filename loc)])
        `#(,(and filename (filename->s-exp filename))
           ,(position->s-exp (source-location-start loc))
           ,(position->s-exp (source-location-end loc))))))

  (define/who s-exp->source-location
    (lambda (e)
      (match e
        [#(,filename ,start ,end)
         (make-source-location (and filename (s-exp->filename filename))
                               (s-exp->position start)
                               (s-exp->position end))]
        [,e (assertion-violation who "invalid source location datum argument" e)])))

  ;; Record writers

  (record-writer (record-type-descriptor source-location)
    (lambda (r p wr)
      (put-string p "#<source-location ")
      (wr (format-source-location r) p)
      (put-string p ">")))

  )
