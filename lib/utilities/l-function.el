;;; package --- Summary ;;; -*- lexical-binding: t; l-syntax: t; -*-

;; Copyright (C) 2025 Laura Viglioni

;; Author: Laura Viglioni
;; Keywords: lisp, functional, programming, utilities
;; URL: https://github.com/viglioni/l-el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a sublibrary of l.el providing additional functionality.
;; This file uses l-syntax
;;
;; since 0.3.3

;;; Code:

(require 'l-main)

(defmacro lcomp (&rest fns)
  "`lcomp' provides function composition right to left.

Function composition allows you to combine multiple functions into a single
function, where the output of one function becomes the input of the next.
The composition is applied right to left, meaning the rightmost function
is applied first.

Each argument is automatically wrapped with `__', so you can use the `__'
placeholder syntax directly: (lcomp (+ __ 1) (* __ 2)) instead of
\(lcomp (__ (+ __ 1)) (__ (* __ 2))).

since: 0.3.3

Arguments:
- No arguments: returns identity function
- One function: returns the function unchanged
- Two or more functions: returns composed function

Examples:
Basic composition with lambdas:
\(ldef double (l x -> (* 2 x)))
\(ldef add-one (l x -> (+ 1 x)))
\(funcall (lcomp add-one double) 5) ;; => 11

Using __ placeholder syntax:
\(funcall (lcomp (+ __ 1) (* __ 2)) 5) ;; => 11

With `with-l' for cleaner syntax:
\(with-l ((lcomp (+ __ 1) (* __ 2)) 5)) ;; => 11

Multiple function composition:
\(with-l ((lcomp (l x -> (+ x 1)) (* __ 2) (* __ 3)) 10)) ;; => 61

String processing example:
\(with-l ((lcomp upcase string-trim) \"  hello  \")) ;; => \"HELLO\"

With local bindings:
\(let ((square (lambda (x) (* x x))))
  (with-l ((lcomp (+ __ 1) square) 3))) ;; => 10"
  `(l--comp ,@(mapcar (lambda (x) `(__ ,x)) fns)))


@doc "Internal function for `lcomp'. Composes functions right to left."
(ldef l--comp () (l x -> x))
(ldef l--comp ((f :function)) f)
(ldef l--comp ((f :function) (g :function))
      `(lambda (&rest args) (funcall (quote ,f) (apply (quote ,g) args))))
(ldef l--comp ((f :function) (g :function) (fn-list :rest))
      (apply 'l--comp (l--comp f g) fn-list))

(provide 'l-function)
;;; l-function.el ends here

