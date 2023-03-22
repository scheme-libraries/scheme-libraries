#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries testing)
  (export
    test-begin
    test-end
    test-assert
    test-eqv
    test-eq
    test-equal
    test-approximate
    test-error
    test-apply
    test-with-runner
    test-match-nth
    test-match-all
    test-match-any
    test-match-name
    test-skip
    test-expect-fail
    test-read-eval-string
    test-runner-group-path
    test-group
    test-group-with-cleanup
    test-result-ref
    test-result-set!
    test-result-clear
    test-result-remove
    test-result-kind
    test-passed?
    test-runner?
    test-runner-reset
    test-runner-null
    test-runner-simple
    test-runner-current
    test-runner-factory
    test-runner-get
    test-runner-create
    test-runner-test-name
    test-runner-pass-count
    test-runner-fail-count
    test-runner-xpass-count
    test-runner-xfail-count
    test-runner-skip-count
    test-runner-group-stack
    test-runner-on-test-begin
    test-runner-on-test-begin!
    test-runner-on-test-end
    test-runner-on-test-end!
    test-runner-on-group-begin
    test-runner-on-group-begin!
    test-runner-on-group-end
    test-runner-on-group-end!
    test-runner-on-final
    test-runner-on-final!
    test-runner-on-bad-count
    test-runner-on-bad-count!
    test-runner-on-bad-end-name
    test-runner-on-bad-end-name!
    test-result-alist
    test-runner-aux-value
    test-runner-aux-value!
    test-on-test-end-simple
    test-on-group-begin-simple
    test-on-group-end-simple
    test-on-bad-count-simple
    test-on-bad-end-name-simple
    test-on-final-simple)
  (import
    (rnrs)
    (rnrs eval)
    (rnrs mutable-pairs)
    (scheme-libraries define-who)
    (scheme-libraries thread-parameters))

  (define-record-type test-runner
    (nongenerative test-runner-d8d8d5f3-9f3b-4314-ae64-d87843729e16)
    (sealed #t) (opaque #t)
    (fields
      (mutable on-test-begin)
      (mutable on-test-end)
      (mutable on-group-begin)
      (mutable on-group-end)
      (mutable on-bad-count)
      (mutable on-bad-end-name)
      (mutable on-final)
      (mutable alist test-result-alist test-result-alist-set!)
      (mutable pass-count)
      (mutable fail-count)
      (mutable xpass-count)
      (mutable xfail-count)
      (mutable skip-count)
      (mutable group-stack)
      (mutable aux-value test-runner-aux-value test-runner-aux-value!)
      (mutable count-list)
      (mutable run-list)
      (mutable skip-list)
      (mutable fail-list)
      (mutable skip-save)
      (mutable fail-save)
      (mutable total-count))
    (protocol
      (lambda (new)
        (lambda (on-test-begin on-test-end on-group-begin on-group-end on-bad-count on-bad-end-name on-final)
          (new on-test-begin on-test-end on-group-begin on-group-end on-bad-count on-bad-end-name on-final
               '() 0 0 0 0 0 '() #f '() #t '() '() '() '() 0)))))

  (define/who test-runner-reset
    (lambda (runner)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (test-result-alist-set! runner '())
      (test-runner-pass-count-set! runner 0)
      (test-runner-fail-count-set! runner 0)
      (test-runner-xpass-count-set! runner 0)
      (test-runner-xfail-count-set! runner 0)
      (test-runner-skip-count-set! runner 0)
      (test-runner-group-stack-set! runner '())
      (test-runner-count-list-set! runner '())
      (test-runner-run-list-set! runner #t)
      (test-runner-skip-list-set! runner '())
      (test-runner-skip-save-set! runner '())
      (test-runner-fail-save-set! runner '())
      (test-runner-total-count-set! runner 0)))

  (define/who test-runner-group-path
    (lambda (runner)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (reverse (test-runner-group-stack runner))))

  (define test-runner-count
    (lambda (runner)
      (+ (test-runner-pass-count runner)
         (test-runner-fail-count runner)
         (test-runner-xpass-count runner)
         (test-runner-xfail-count runner)
         (test-runner-skip-count runner))))

  (define/who test-on-test-begin-simple
    (lambda (runner)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))))

  (define/who test-on-test-end-simple
    (lambda (runner)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (let ([kind (test-result-kind runner)])
        (case kind
          [(pass xpass skip)
           (put-string output-port "ok ")]
          [(fail xfail)
           (put-string output-port "not ok ")]
          [else (assert #f)])
        (put-string output-port
                    (number->string (test-runner-count runner)))
        (case kind
          [(xfail xpass)
           (put-string output-port " # TODO")]
          [(skip)
           (put-string output-port " # SKIP")])
        (let ([test-name (test-runner-test-name runner)])
          (unless (string=? test-name "")
            (case kind
              [(pass fail)
               (put-string output-port " -")])
            (put-string output-port " ")
            (put-string output-port test-name)))
        (put-char output-port #\newline))))

  (define/who test-on-group-begin-simple
    (lambda (runner suite-name count)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (string? suite-name)
        (assertion-violation who "invalid suite name argument" suite-name))
      (unless (or (not count) (integer? count))
        (assertion-violation who "invalid count argument" count))
      (when (null? (test-runner-group-stack runner))
        (put-string output-port "# Starting test ")
        (put-string output-port suite-name)
        (put-char output-port #\newline))))

  (define/who test-on-group-end-simple
    (lambda (runner)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))))

  (define/who test-on-bad-count-simple
    (lambda (runner actual-count expected-count)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (integer? actual-count)
        (assertion-violation who "invalid actual count argument" actual-count))
      (unless (integer? expected-count)
        (assertion-violation who "invalid expected count argument" expected-count))
      (assertion-violation who
                           "expected count does not match actual count"
                           expected-count actual-count)))

  (define/who test-on-bad-end-name-simple
    (lambda (runner begin-name end-name)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (string? begin-name)
        (assertion-violation who "invalid begin name argument" begin-name))
      (unless (integer? end-name)
        (assertion-violation who "invalid end name argument" end-name))
      (assertion-violation who
                           "test-end does not match test-begin"
                           end-name begin-name)))

  (define/who test-on-final-simple
    (lambda (runner)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (put-string output-port "1..")
      (put-string output-port (number->string (test-runner-count runner)))
      (put-char output-port #\newline)))

  (define test-runner-simple
    (lambda ()
      (make-test-runner test-on-test-begin-simple
                        test-on-test-end-simple
                        test-on-group-begin-simple
                        test-on-group-end-simple
                        test-on-bad-count-simple
                        test-on-bad-end-name-simple
                        test-on-final-simple)))

  (define test-runner-null
    (lambda ()
      (make-test-runner (lambda (runner) (values))
                        (lambda (runner) (values))
                        (lambda (runner suite-name count) (values))
                        (lambda (runner) (values))
                        (lambda (runner actual-count expected-count) (values))
                        (lambda (runner begin-name end-name) (values))
                        (lambda (runner) (values)))))

  (define/who test-runner-current
    (make-thread-parameter #f
                           (lambda (runner)
                             (unless (or (not runner)
                                         (test-runner? runner))
                               (invalid-test-runner-violation who runner))
                             runner)))

  (define/who test-runner-get
    (lambda ()
      (or (test-runner-current)
          (assertion-violation who "test runner not initialized"))))

  (define/who test-runner-factory
    (make-thread-parameter test-runner-simple
                           (lambda (factory)
                             (unless (procedure? factory)
                               (assertion-violation who "invalid factory argument" factory))
                             factory)))

  (define test-runner-create
    (lambda ()
      ((test-runner-factory))))

  (define/who test-begin
    (case-lambda
      [(suite-name count)
       (unless (string? suite-name)
         (assertion-violation who "invalid suite name argument" suite-name))
       (unless (or (not count) (integer? count))
         (assertion-violation who "invalid count argument" count))
       (or (test-runner-current) (test-runner-current (test-runner-create)))
       (let ([runner (test-runner-current)])
         ((test-runner-on-group-begin runner) runner suite-name count)
         (test-runner-skip-save-set! runner
			             (cons (test-runner-skip-list runner)
				           (test-runner-skip-save runner)))
         (test-runner-fail-save-set! runner
			             (cons (test-runner-fail-list runner)
				           (test-runner-fail-save runner)))
         (test-runner-count-list-set! runner
			              (cons (cons (test-runner-total-count runner) count)
				            (test-runner-count-list runner)))
         (test-runner-group-stack-set! runner
				       (cons suite-name
				             (test-runner-group-stack runner))))]
      [(suite-name)
       (test-begin suite-name #f)]))

  (define/who test-end
    (case-lambda
      [(suite-name)
       (unless (or (not suite-name) (string? suite-name))
         (assertion-violation who "invalid suite name argument" suite-name))
       (let* ([runner (test-runner-get)]
	      [groups (test-runner-group-stack runner)])
         (test-result-alist-set! runner '())
         (when (null? groups)
	   (assertion-violation who "not in a group"))
         (when (and suite-name (not (equal? suite-name (car groups))))
	   ((test-runner-on-bad-end-name runner) runner suite-name (car groups)))
         (let* ([count-list (test-runner-count-list runner)]
	        [expected-count (cdar count-list)]
	        [saved-count (caar count-list)]
	        [group-count (- (test-runner-total-count runner) saved-count)])
	   (when (and expected-count (not (= expected-count group-count)))
	     ((test-runner-on-bad-count runner) runner group-count expected-count))
	   ((test-runner-on-group-end runner) runner)
	   (test-runner-group-stack-set! runner (cdr (test-runner-group-stack runner)))
	   (test-runner-skip-list-set! runner (car (test-runner-skip-save runner)))
	   (test-runner-skip-save-set! runner (cdr (test-runner-skip-save runner)))
	   (test-runner-fail-list-set! runner (car (test-runner-fail-save runner)))
	   (test-runner-fail-save-set! runner (cdr (test-runner-fail-save runner)))
	   (test-runner-count-list-set! runner (cdr count-list))
	   (when (null? (test-runner-group-stack runner))
	     ((test-runner-on-final runner) runner))))]
      [()
       (test-end #f)]))

  (define-syntax/who test-group
    (lambda (x)
      (syntax-case x ()
        [(_ suite-name body1 ... body2)
         #'(let ([runner (test-runner-current)]
	         [name suite-name])
             (test-result-alist-set! runner `((test-name . ,name)))
             (when (test-should-execute runner)
	       (dynamic-wind
	         (lambda () (test-begin name))
	         (lambda () body1 ... body2)
	         (lambda () (test-end name)))))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax/who test-group-with-cleanup
    (lambda (x)
      (syntax-case x ()
        [(_ suite-name body1 ... body2 cleanup-form)
         #'(test-group suite-name
             (dynamic-wind
               (lambda () (values))
               (lambda () body1 ... body2)
               (lambda () cleanup-form)))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define/who test-result-ref
    (case-lambda
      [(runner property-name default)
       (unless (test-runner? runner)
         (invalid-test-runner-violation who runner))
       (unless (symbol? property-name)
         (assertion-violation "invalid property name argument" property-name))
       (cond
        [(assq property-name (test-result-alist runner)) => cdr]
        [else default])]
      [(runner property-name)
       (test-result-ref runner property-name #f)]))

  (define/who test-result-set!
    (lambda (runner property-name value)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (symbol? property-name)
        (assertion-violation "invalid property name argument" property-name))
      (let ([alist (test-result-alist runner)])
        (cond
         [(assq property-name alist)
          => (lambda (pair)
	       (set-cdr! pair value))]
         [else (test-result-alist-set! runner (cons (cons property-name value) alist))]))))

  (define/who test-runner-test-name
    (lambda (runner)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (test-result-ref runner 'test-name "")))

  (define/who test-result-clear
    (lambda (runner)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (test-result-alist-set! runner '())))

  (define/who test-result-remove
    (lambda (runner property-name)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (symbol? property-name)
        (assertion-violation "invalid property name argument" property-name))
      (test-result-alist-set! runner
		              (let f ([alist (test-result-alist runner)])
			        (cond
			         [(null? alist)
			          '()]
			         [(eq? (caar alist) property-name) (cdr alist)]
			         [else (cons (car alist) (f (cdr alist)))])))))

  (define/who test-result-kind
    (case-lambda
      [(runner)
       (unless (test-runner? runner)
         (invalid-test-runner-violation who runner))
       (test-result-ref runner 'result-kind)]
      [()
       (test-result-kind (test-runner-current))]))

  (define/who test-passed?
    (case-lambda
      [(runner)
       (unless (test-runner? runner)
         (invalid-test-runner-violation who runner))
       (case (test-result-ref runner 'result-kind)
         [(pass xpass) #t]
         [else #f])]
      [()
       (test-passed? (test-runner-get))]))

  (define test-specifier-matches
    (lambda (specifier runner)
      (specifier runner)))

  (define test-any-specifier-matches
    (lambda (list runner)
      (let ([result #f])
        (for-each
         (lambda (specifier)
           (when (test-specifier-matches specifier runner)
	     (set! result #t)))
         list)
        result)))

  (define test-should-execute
    (lambda (runner)
      (define run (test-runner-run-list runner))
      (cond
       [(or (not (or (eq? run #t)
		     (test-any-specifier-matches run runner)))
	    (test-any-specifier-matches (test-runner-skip-list runner) runner))
        (test-result-set! runner 'result-kind 'skip)
        #f]
       [(test-any-specifier-matches (test-runner-fail-list runner) runner)
        (test-result-set! runner 'result-kind 'xfail)
        'xfail]
       [else #t])))

  (define test-on-test-begin
    (lambda (runner)
      (test-should-execute runner)
      ((test-runner-on-test-begin runner) runner)
      (not (eq? 'skip (test-result-ref runner 'result-kind)))))

  (define test-on-test-end
    (lambda (runner result)
      (test-result-set! runner
		        'result-kind
		        (if (eq? (test-result-ref runner 'result-kind) 'xfail)
			    (if result 'xpass 'xfail)
			    (if result 'pass 'fail)))))

  (define-syntax test-evaluate-with-catch
    (syntax-rules ()
      [(test-evaluate-with-catch test-expression)
       (guard (condition (else
			  (test-result-set! (test-runner-current) 'actual-error
					    condition)
			  #f))
         test-expression)]))

  (define test-report-result
    (lambda ()
      (define runner (test-runner-get))
      (define result-kind (test-result-kind runner))
      (case result-kind
        [(pass)
         (test-runner-pass-count-set! runner (+ (test-runner-pass-count runner) 1))]
        [(fail)
         (test-runner-fail-count-set! runner (+ (test-runner-fail-count runner) 1))]
        [(xpass)
         (test-runner-xpass-count-set! runner (+ (test-runner-xpass-count runner) 1))]
        [(xfail)
         (test-runner-xfail-count-set! runner (+ (test-runner-xfail-count runner) 1))]
        [else
         (test-runner-skip-count-set! runner (+ (test-runner-skip-count runner) 1))])
      (test-runner-total-count-set! runner (+ (test-runner-total-count runner) 1))
      ((test-runner-on-test-end runner) runner)))

  (define-syntax test-comp1body
    (syntax-rules ()
      [(test-comp1body runner expression)
       (begin
         (when (test-on-test-begin runner)
	   (let ((result (test-evaluate-with-catch expression)))
	     (test-result-set! runner 'actual-value result)
	     (test-on-test-end runner result)))
         (test-report-result))]))

  (define-syntax test-comp2body
    (syntax-rules ()
      [(test-body runner compare expected expression)
       (begin
         (when (test-on-test-begin runner)
	   (let ([e expected])
	     (test-result-set! runner 'expected-value e)
	     (let ([result (test-evaluate-with-catch expression)])
	       (test-result-set! runner 'actual-value result)
	       (test-on-test-end runner (compare e result)))))
         (test-report-result))]))

  (define test-approximate=
    (let ([who 'test-approximate])
      (lambda (error)
        (unless (real? error)
          (assertion-violation who "invalid error argument" error))
        (lambda (value expected)
          (unless (real? value)
            (assertion-violation who "invalid value argument" value))
          (unless (real? expected)
            (assertion-violation who "invalid expected argument" expected))
          (<= (- expected error) value (+ expected error))))))

  (define-syntax/who test-assert
    (lambda (x)
      (syntax-case x ()
        [(_ name test-expression)
         #'(let ([runner (test-runner-get)])
             (test-result-alist-set! runner `((test-name . ,name)))
             (test-comp1body runner test-expression))]
        [(_ test-expression)
         #'(let ([runner (test-runner-get)])
             (test-result-alist-set! runner '())
             (test-comp1body runner test-expression))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax/who test-equal
    (lambda (x)
      (syntax-case x ()
        [(_ name expected actual)
         #'(test-comp2 equal? name expected actual)]
        [(_ expected actual)
         #'(test-comp2 equal? expected actual)]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax/who test-eqv
    (lambda (x)
      (syntax-case x ()
        [(_ name expected actual)
         #'(test-comp2 eqv? name expected actual)]
        [(_ expected actual)
         #'(test-comp2 eqv? expected actual)]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax/who test-eq
    (lambda (x)
      (syntax-case x ()
        [(_ name expected actual)
         #'(test-comp2 eq? name expected actual)]
        [(_ expected actual)
         #'(test-comp2 eq? expected actual)]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax/who test-approximate
    (lambda (x)
      (syntax-case x ()
        [(_ name expected actual)
         #'(test-comp2 (test-approximate= error) name expected actual error)]
        [(_ expected actual error)
         #'(test-comp2 (test-approximate= error) expected actual)]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define-syntax test-comp2
    (syntax-rules ()
      [(test-comp2 compare name expected expression)
       (let ((runner (test-runner-get)))
         (test-result-alist-set! runner `((test-name . ,name)))
         (test-comp2body runner compare expected expression))]
      [(test-comp2 compare expected expression)
       (let ((runner (test-runner-get)))
         (test-result-alist-set! runner '())
         (test-comp2body runner compare expected expression))]))

  (define-syntax/who test-error
    (lambda (x)
      (syntax-case x ()
        [(_ name error-type expression)
         #'(let ([runner (test-runner-get)])
             (test-result-alist-set! runner `((test-name . ,name)))
             (test-comp1body runner (guard (condition (error-type #t)) expression #f)))         ]
        [(_ error-type expression)
         #'(let ([runner (test-runner-get)])
             (test-result-alist-set! runner '())
             (test-comp1body runner (guard (condition (error-type #t)) expression #f)))]
        [(_ expression)
         #'(test-error #t expression)]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define/who (test-read-eval-string string)
    (unless (string? string)
      (assertion-violation who "invalid string argument" string))
    (let ([port (open-string-input-port string)])
      (define form (get-datum port))
      (if (eof-object? (lookahead-char port))
          (eval form (environment '(rnrs)))
          (error who "extra junk after read" (get-string-all port)))))

  (define-syntax/who test-with-runner
    (lambda (x)
      (syntax-case x ()
        [(test-with-runner runner body1 ... body2)
         #'(let ([saved-runner (test-runner-current)])
             (dynamic-wind
	       (lambda () (test-runner-current runner))
	       (lambda () body1 ... body2)
	       (lambda () (test-runner-current saved-runner))))]
        [_ (syntax-violation who "invalid syntax" x)])))

  (define/who test-apply
    (define doapply
      (lambda (first rest)
        (define runner (test-runner-current))
	(if runner
            (doapply/runner runner first rest)
            (let ([runner (test-runner-create)])
              (test-with-runner runner (doapply/runner runner first rest))
              ((test-runner-on-final runner) runner)))))
    (define doapply/runner
      (lambda (runner first rest)
        (let f ([first first] [rest rest])
          (let ([run-list (test-runner-run-list runner)])
            (cond
             [(null? rest)
              (test-runner-run-list-set! runner (reverse run-list))
	      (first)]
             [else
	      (test-runner-run-list-set! runner
			                 (if (eq? run-list #t)
				             (list first)
				             (cons first run-list)))
	      (f (car rest) (cdr rest))
	      (test-runner-run-list-set! runner run-list)])))))
    (case-lambda
      [(runner first . rest)
       (if (test-runner? runner)
           (test-with-runner runner (doapply/runner runner first rest))
           (doapply runner (cons first rest)))]
      [(first . rest)
       (doapply first rest)]))

  (define/who test-match-nth
    (case-lambda
      [(n count)
       (unless (integer? n)
         (assertion-violation who "invalid number argument" n))
       (unless (integer? count)
         (assertion-violation who "invalid count argument" count))
       (let ([i 0])
         (lambda (runner)
	   (set! i (+ i 1))
	   (and (>= i n) (< i (+ n count)))))]
      [(n)
       (test-match-nth n 1)]))

  (define test-match-all
    (lambda (predicate*)
      (define specifier*
        (map test-as-specifier predicate*))
      (lambda (runner)
        (define result #t)
        (let loop ((specifier* specifier*))
          (cond
           ((null? specifier*)
	    result)
           (else
	    (unless ((car specifier*) runner)
	      (set! result #f))
	    (loop (cdr specifier*))))))))

  (define (test-match-any . predicate*)
    (define specifier* (map test-as-specifier predicate*))
    (lambda (runner)
      (define result #f)
      (for-each
       (lambda (specifier)
         (when (specifier runner)
           (set! result #t)))
       specifier*)
      result))

  (define/who test-match-name
    (lambda (name)
      (unless (or (not name) (string? name))
        (assertion-violation who "invalid name argument" name))
      (lambda (runner)
        (equal? name (test-runner-test-name runner)))))

  (define test-skip
    (lambda (predicate)
      (define runner (test-runner-get))
      (test-runner-skip-list-set! runner
			          (cons (test-match-all predicate)
				        (test-runner-skip-list runner)))))

  (define test-expect-fail
    (lambda (predicate)
      (define runner (test-runner-get))
      (test-runner-fail-list-set! runner
			          (cons (test-match-all predicate)
				        (test-runner-fail-list runner)))))

  (define test-as-specifier
    (lambda (specifier)
      (cond
       [(procedure? specifier) specifier]
       [(integer? specifier) (test-match-nth 1 specifier)]
       [(string? specifier) (test-match-name specifier)]
       [else (assertion-violation 'test-specifier "invalid test specifier" specifier)])))

  (define/who test-runner-on-test-begin!
    (lambda (runner callback)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (procedure? callback)
        (invalid-callback-procedure-violation who callback))
      (test-runner-on-test-begin-set! runner callback)))

  (define/who test-runner-on-test-end!
    (lambda (runner callback)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (procedure? callback)
        (invalid-callback-procedure-violation who callback))
      (test-runner-on-test-end-set! runner callback)))

  (define/who test-runner-on-group-begin!
    (lambda (runner callback)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (procedure? callback)
        (invalid-callback-procedure-violation who callback))
      (test-runner-on-group-begin-set! runner callback)))

  (define/who test-runner-on-group-end!
    (lambda (runner callback)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (procedure? callback)
        (invalid-callback-procedure-violation who callback))
      (test-runner-on-group-end-set! runner callback)))

  (define/who test-runner-on-bad-count!
    (lambda (runner callback)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (procedure? callback)
        (invalid-callback-procedure-violation who callback))
      (test-runner-on-bad-count-set! runner callback)))

  (define/who test-runner-on-bad-end-name!
    (lambda (runner callback)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (procedure? callback)
        (invalid-callback-procedure-violation who callback))
      (test-runner-on-bad-end-name-set! runner callback)))

  (define/who test-runner-on-final!
    (lambda (runner callback)
      (unless (test-runner? runner)
        (invalid-test-runner-violation who runner))
      (unless (procedure? callback)
        (invalid-callback-procedure-violation who callback))
      (test-runner-on-final-set! runner callback)))

  (define invalid-test-runner-violation
    (lambda (who what)
      (assertion-violation who "invalid test runner argument" what)))

  (define invalid-callback-procedure-violation
    (lambda (who what)
      (assertion-violation who "invalid callback procedure argument" what)))

  (define output-port
    (transcoded-port (standard-output-port)
                     (make-transcoder (utf-8-codec)
                                      (eol-style lf)))))
