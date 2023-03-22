;; Copyright © Marc Nieper-Wißkirchen (2022).

;; This file is part of Egg.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
	  (put 'with-implicit 'scheme-indent-function 1)
	  (put 'with-syntax 'scheme-indent-function 1)
	  (font-lock-add-keywords
	   nil
	   '(("(\\(define/who\\|define-record-type\\|define-syntax/who\\|define-values\\|define-auxiliary-syntax\\|define-language\\)\\>[ \t]*(*\\(\\sw+\\)?"
              (1 font-lock-keyword-face)
              (2 font-lock-function-name-face nil t))
	     ("(\\(extends\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(entry\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(terminals\\)\\>" 1 font-lock-keyword-face)
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
	     ("(\\(with-syntax\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(loop\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(rec\\)\\>" 1 font-lock-keyword-face)
	     ("(\\(syntax-case\\)\\>" 1 font-lock-keyword-face))))))))
