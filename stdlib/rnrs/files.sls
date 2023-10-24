#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs files (6))
  (export
    file-exists?
    delete-file)
  (import
    ($system)))
