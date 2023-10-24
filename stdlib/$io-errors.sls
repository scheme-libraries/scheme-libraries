#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library ($io-errors)
  (export
    &i/o
    make-i/o-error
    i/o-error?
    &i/o-read
    make-i/o-read-error
    i/o-read-error?
    &i/o-write
    make-i/o-write-error
    i/o-write-error?
    &i/o-invalid-position
    make-i/o-invalid-position-error
    i/o-invalid-position-error?
    i/o-error-position
    &i/o-filename
    make-i/o-filename-error
    i/o-filename-error?
    i/o-error-filename
    &i/o-file-protection
    make-i/o-file-protection-error
    i/o-file-protection-error?
    &i/o-file-is-read-only
    make-i/o-file-is-read-only-error
    i/o-file-is-read-only-error?
    &i/o-file-already-exists
    make-i/o-file-already-exists-error
    i/o-file-already-exists-error?
    &i/o-file-does-not-exist
    make-i/o-file-does-not-exist-error
    i/o-file-does-not-exist-error?
    &i/o-port
    make-i/o-port-error
    i/o-port-error?
    i/o-error-port)
  (import
    ($system)
    ($condition-names))

  (define-condition-name &i/o (i/o-rtd))
  (define-condition-name &i/o-read (i/o-read-rtd))
  (define-condition-name &i/o-write (i/o-write-rtd))
  (define-condition-name &i/o-invalid-position (i/o-invalid-position-rtd))
  (define-condition-name &i/o-filename (i/o-filename-rtd))
  (define-condition-name &i/o-file-protection (i/o-file-protection-rtd))
  (define-condition-name &i/o-file-is-read-only (i/o-file-is-read-only-rtd))
  (define-condition-name &i/o-file-already-exists (i/o-file-already-exists-rtd))
  (define-condition-name &i/o-file-does-not-exist (i/o-file-does-not-exist-rtd))
  (define-condition-name &i/o-port (i/o-port-rtd))


  )
