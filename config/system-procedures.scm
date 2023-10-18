#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

((scheme-libraries syntax expressions $runtime)

 ;; (rnrs base)
 eqv?
 eq?
 equal?
 number?
 complex?
 real?
 rational?
 integer?
 real-valued?
 rational-valued?
 integer-valued?
 exact?
 inexact?
 exact
 inexact
 =
 <
 >
 <=
 >=
 zero?
 positive?
 negative?
 odd?
 even?
 finite?
 infinite?
 nan?
 max
 min
 +
 *
 -
 /
 abs
 div-and-mod
 div
 mod
 div0-and-mod0
 div0
 mod0
 gcd
 lcm
 numerator
 denominator
 floor
 ceiling
 truncate
 round
 rationalize
 exp
 log
 sin
 cos
 tan
 asin
 acos
 atan
 sqrt
 exact-integer-sqrt
 expt
 make-rectangular
 make-polar
 real-part
 imag-part
 magnitude
 angle
 number->string
 string->number
 not
 boolean?
 boolean=?
 pair?
 cons
 car
 cdr
 caar
 cadr
 cdar
 cddr
 caaaar
 caaadr
 caaar
 caadar
 caaddr
 caadr
 cadaar
 cadadr
 cadar
 caddar
 cadddr
 caddr
 cdaaar
 cdaadr
 cdaar
 cdadar
 cdaddr
 cdadr
 cddaar
 cddadr
 cddar
 cdddar
 cddddr
 cdddr
 null?
 list?
 list
 length
 append
 reverse
 list-tail
 list-ref
 map
 for-each
 symbol?
 symbol->string
 symbol=?
 string->symbol
 char?
 char->integer
 integer->char
 char=?
 char<?
 char>?
 char<=?
 char>=?
 string?
 make-string
 string
 string-length
 string-ref
 string=?
 string<?
 string>?
 string<=?
 string>=?
 substring
 string-append
 string->list
 list->string
 string-for-each
 string-copy
 vector?
 make-vector
 vector
 vector-length
 vector-ref
 vector-set!
 vector->list
 list->vector
 vector-fill!
 vector-map
 vector-for-each
 error
 assertion-violation
 apply
 call-with-current-continuation
 call/cc
 values
 call-with-values
 dynamic-wind

 ;; (rnrs unicode)
 char-upcase
 char-downcase
 char-titlecase
 char-foldcase
 char-ci=?
 char-ci<?
 char-ci>?
 char-ci<=?
 char-ci>=?
 char-alphabetic?
 char-numeric?
 char-whitespace?
 char-upper-case?
 char-lower-case?
 char-title-case?
 char-general-category
 string-upcase
 string-downcase
 string-titlecase
 string-foldcase
 string-ci=?
 string-ci<?
 string-ci>?
 string-ci<=?
 string-ci>=?
 string-normalize-nfd
 string-normalize-nfkd
 string-normalize-nfc
 string-normalize-nfkc

 ;; (rnrs bytevectors)
 native-endianness
 bytevector?
 make-bytevector
 bytevector-length
 bytevector=?
 bytevector-fill!
 bytevector-copy!
 bytevector-copy

 ;; (rnrs lists)
 find
 for-all
 exists
 filter
 partition
 fold-left
 fold-right
 remp
 remove
 remv
 remq
 memp
 member
 memv
 memq
 assp
 assoc
 assv
 assq
 cons*

 ;; (rnrs syntax-case)
 make-variable-transformer
 identifier?
 bound-identifier=?
 free-identifier=?
 syntax->datum
 datum->syntax
 generate-temporaries

 ;; (rnrs enums)
 make-enumeration
 enum-set-universe
 enum-set-indexer
 enum-set-constructor
 enum-set->list
 enum-set-member?
 enum-set-subset?
 enum-set=?
 enum-set-union
 enum-set-intersection
 enum-set-difference
 enum-set-complement
 enum-set-projection

 display
 newline)
