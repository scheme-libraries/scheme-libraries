#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries $config)
  (export
    random-source)
  (import
    (rnrs))

  (define random-source "/dev/urandom"))
