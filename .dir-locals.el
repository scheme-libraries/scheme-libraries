;; Copyright © Marc Nieper-Wißkirchen (2023).

((scheme-mode
  . ((lisp-indent-offset . nil)
     (eval
      . (progn
          (put 'export 'scheme-indent-function 0)
          (put 'import 'scheme-indent-function 0)
          (put 'fields 'scheme-indent-function 0)
          (put 'protocol 'scheme-indent-function 0)
	  (put 'guard 'scheme-indent-function 1)
	  (put 'rec 'scheme-indent-function 1)
          (put 'test-datums 'scheme-indent-function 'defun)
	  (put 'with-implicit 'scheme-indent-function 1)
	  (put 'with-syntax 'scheme-indent-function 1)
	  (font-lock-add-keywords
	   nil
	   '(("(\\(define/who\\|define-record-type\\|define-syntax/who\\|define-values\\|define-auxiliary-syntax\\)\\>[ \t]*(*\\(\\sw+\\)?"
              (1 font-lock-keyword-face)
              (2 font-lock-function-name-face nil t))
	     ("(\\(fields\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(protocol\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(sealed\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(opaque\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(nongenerative\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(parent\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(mutable\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(immutable\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(increment!\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(list-case\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(match\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(loop\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(rec\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(syntax-case\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(test-datums\\)\\>" 1 font-lock-keyword-face))))))))
