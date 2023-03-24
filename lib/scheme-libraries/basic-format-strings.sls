#!r6rs

;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (scheme-libraries basic-format-strings)
  (export
    format)
  (import
    (rnrs)
    (scheme-libraries define-who))

  (define/who format
    (lambda (cntl . arg*)
      (define not-enough-values-available-violation
        (lambda ()
          (assertion-violation who "not enough values available for control string" cntl)))
      (unless (string? cntl)
        (assertion-violation who "invalid control string argument" cntl))
      (let-values ([(p extract) (open-string-output-port)])
        (define n (string-length cntl))
        (let f ([i 0] [arg* arg*])
	  (cond
	   [(< i n)
	    (let ([c (string-ref cntl i)] [i (+ i 1)])
	      (cond
	       [(and (char=? c #\~) (< i n))
	        (case (string-ref cntl i)
		  [(#\a #\A)
                   (when (null? arg*)
                     (not-enough-values-available-violation))
		   (display (car arg*) p)
		   (f (+ i 1) (cdr arg*))]
		  [(#\s #\S)
                   (when (null? arg*)
                     (not-enough-values-available-violation))
		   (write (car arg*) p)
		   (f (+ i 1) (cdr arg*))]
		  [(#\%)
		   (newline p)
		   (f (+ i 1) arg*)]
		  [(#\~)
		   (put-char p #\~)
		   (f (+ i 1) arg*)]
		  [else
		   (put-char p c)
		   (f i arg*)])]
	       [else
                (put-char p c)
                (f i arg*)]))]
           [else
            (unless (null? arg*)
              (apply assertion-violation who "too many values available for control string" cntl arg*))
            (extract)]))))))
