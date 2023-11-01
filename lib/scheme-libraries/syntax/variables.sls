#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries syntax variables)
  (export
    make-variable
    variable?
    variable=?
    variable->symbol
    symbol->variable
    make-variable-hashtable
    variable-hash)
  (import
    (rnrs)
    (scheme-libraries define-who)
    (scheme-libraries uuid)
    (scheme-libraries record-writer))

  #;
  (define-record-type variable
    (nongenerative variable-7c0fd95e-43aa-46e4-9d67-2fc37ad6b4ef)
    (fields name (mutable symbol))
    (protocol
      (lambda (new)
        (define who 'make-variable)
        (lambda (name)
          (unless (symbol? name)
            (assertion-violation who "invalid name argument" name))
          (new name #f)))))

  (define make-variable
    (lambda (name)
      (uid name)))

  (define variable?
    (lambda (x)
      (symbol? x)))

  (define variable=?
    (lambda (x y)
      (symbol=? x y)))

  #;
  (define/who variable=?
    (lambda (var1 var2)
      (unless (variable? var1)
        (assertion-violation who "invalid first variable argument" var1))
      (unless (variable? var2)
        (assertion-violation who "invalid second variable argument" var2))
      (or (eq? var1 var2)
          (let ([sym1 (variable-symbol var1)]
                [sym2 (variable-symbol var2)])
            (and sym1 sym2 (symbol=? sym1 sym2))))))

  (define variable-hash
    (lambda (x)
      (symbol-hash x)))

  #;
  (define/who variable-hash
    (lambda (var)
      (unless (variable? var)
        (assertion-violation who "invalid variable argument" var))
      (symbol-hash (variable->symbol var))))

  (define make-variable-hashtable
    (lambda ()
      (make-hashtable variable-hash variable=?)))

  (define variable->symbol
    (lambda (x)
      x))

  #;
  (define/who variable->symbol
    (lambda (var)
      (unless (variable? var)
        (assertion-violation who "invalid variable argument"))
      (or (variable-symbol var)
          (let ([sym (uid (variable-name var))])
            (variable-symbol-set! var sym)
            sym))))

  (define symbol->variable
    (lambda (x)
      x))

  #;
  (define/who symbol->variable
    (lambda (sym)
      (unless (symbol? sym)
        (assertion-violation who "invalid symbol argument" sym))
      (let ([s (symbol->string sym)])
        (let ([n (string-length s)])
          (unless (>= n 37)
            (assertion-violation who "invalid symbol argument" sym))
          (let ([var (make-variable (string->symbol (substring s 0 (- n 37))))])
            (variable-symbol-set! var sym)
            var)))))

  ;; Record writer

  #;
  (record-writer (record-type-descriptor variable)
    (lambda (r p wr)
      (put-string p "#<variable ")
      (put-string p (symbol->string (variable-name r)))
      (put-string p ">"))))
