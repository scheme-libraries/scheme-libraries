;; Copyright © Marc Nieper-Wißkirchen (2023).

((scheme-mode
  . ((lisp-indent-offset . nil)
     (eval
      . (progn
          (put '$with-syntax 'scheme-indent-function 1)
          (put 'values 'scheme-indent-function 0)
          (put 'for-each 'scheme-indent-function 'defun)
          (put 'map 'scheme-indent-function 0)
          (put 'for-all 'scheme-indent-function 0)
          (put 'exists 'scheme-indent-function 0)
          (put 'fold-left 'scheme-indent-function 'defun)
          (put 'fold-right 'scheme-indent-function 'defun)
          (put 'condition 'scheme-indent-function 'defun)
          (put 'with-syntax-error-context 'scheme-indent-function 2)
          (put 'build 'scheme-indent-function 0)
          (put 'build-begin 'scheme-indent-function 0)
          (put 'build-let 'scheme-indent-function 1)
          (put 'test-expand 'scheme-indent-function 'defun)
          (put 'declare-syntax 'scheme-indent-function 1)
          (put 'declare-definition-syntax 'scheme-indent-function 1)
          (put 'declare-expander-syntax 'scheme-indent-function 1)
          (put 'declare-prim-syntax 'scheme-indent-function 1)
          (put 'declare-splicing-syntax 'scheme-indent-function 1)
          (put 'make 'scheme-indent-function nil)
          (put 'make-parameter 'scheme-indent-function 1)
          (put 'make-thread-parameter 'scheme-indent-function 1)
          (put 'format 'scheme-indent-function 1)
          (put 'extend-backquote 'scheme-indent-function 1)
          (put 'syntax-extend-backquote 'scheme-indent-function 1)
          (put 'export 'scheme-indent-function 0)
          (put 'import 'scheme-indent-function 0)
          (put 'fields 'scheme-indent-function 0)
          (put 'protocol 'scheme-indent-function 0)
	  (put 'guard 'scheme-indent-function 1)
	  (put 'rec 'scheme-indent-function 1)
          (put 'test-datums 'scheme-indent-function 'defun)
	  (put 'with-implicit 'scheme-indent-function 1)
	  (put 'with-syntax 'scheme-indent-function 1)
          (put 'syntax-match 'scheme-indent-function 1)
          (put 'syntax-error 'scheme-indent-function 'defun)
	  (font-lock-add-keywords
	   nil
	   '(("(\\(define/who\\|define-record-type\\|define-syntax/who\\|define-values\\|define-auxiliary-syntax\\)\\>[ \t]*(*\\(\\sw+\\)?"
              (1 font-lock-keyword-face)
              (2 font-lock-function-name-face nil t))
             ("(\\(declare-syntax\\|declare-\\(expander\\|definition\\|prim\\|splicing\\)-syntax\\)\\>[ \t]*(*\\(\\sw+\\)?"
              (1 font-lock-keyword-face)
              (2 font-lock-function-name-face nil t))
	     ("(\\(\\(syntax-\\)?extend-backquote\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(build\\(-begin\\|-let\\)?\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(fields\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(protocol\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(sealed\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(opaque\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(nongenerative\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(parent\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(mutable\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(immutable\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(assert\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(increment!\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(prepend!\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(match\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(loop\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(rec\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(syntax-case\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(syntax-match\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(with-syntax\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(with-syntax-error-context\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(test-datums\\)\\>" 1 font-lock-keyword-face))))))))
