#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (rnrs arithmetic flonums)
  (export
    flonum?
    real->flonum
    fl=?
    fl<?
    fl<=?
    fl>?
    fl>=?
    flinteger?
    flzero?
    flpositive?
    flnegative?
    flodd?
    fleven?
    flfinite?
    flinfinite?
    flnan?
    flmax
    flmin
    fl+
    fl*
    fl-
    fl/
    flabs
    fldiv-and-mod
    fldiv
    flmod
    fldiv0-and-mod0
    fldiv0
    flmod0
    flnumerator
    fldenominator
    flfloor
    flceiling
    fltruncate
    flround
    flexp
    fllog
    flsin
    flcos
    fltan
    flasin
    flacos
    flatan
    flsqrt
    flexpt
    fixnum->flonum
    &no-infinities
    make-no-infinities-violation
    no-infinities-violation?
    &no-nans
    make-no-nans-violation
    no-nans-violation?)
  (import
    ($system)
    ($condition-names))

  (define-condition-name &no-infinities (no-infinities-rtd))
  (define-condition-name &no-nans (no-nans-rtd))

  )
