#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax $syntax-types)
  (export
    syntax-type syntax-type?
    binding binding?
    make-displaced-binding
    displaced-binding?
    make-out-of-phase-binding
    out-of-phase-binding?)
  (import
    (rnrs))

  (define-record-type syntax-type
    (nongenerative syntax-type-5076a7a5-0cce-49cb-9928-256ffdfa7aee))

  (define-record-type binding
    (nongenerative binding-749b4948-3923-484f-b466-3e8a9048a931)
    (parent syntax-type))

  (define-record-type displaced-binding
    (nongenerative displaced-binding-ac5ee8c9-fc00-4e4d-919e-90373972d283)
    (parent binding) (sealed #t))

  (define-record-type out-of-phase-binding
    (nongenerative out-of-phase-binding-9f0102df-6804-4d78-b919-27057da676bc)
    (parent binding)
    (sealed #t))

  )
