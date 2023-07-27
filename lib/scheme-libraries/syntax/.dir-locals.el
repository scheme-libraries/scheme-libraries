;; Copyright © Marc Nieper-Wißkirchen (2023).

((scheme-mode
  . ((lisp-indent-offset . nil)
     (eval
      . (progn
          (put '$library 'scheme-indent-function 'defun)
	  (font-lock-add-keywords
	   nil
	   '(("(\\($library\\)\\>" 1 font-lock-keyword-face))))))))
