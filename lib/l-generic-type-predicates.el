;;; l-generic-type-predicates.el --- Functions for type match pattern -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laura Viglioni

;; Author: Laura Viglioni
;; Keywords: lisp, functional, programming, generics, pattern-matching
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
;; This module provides a list of functions to enable type match.

;;; Code:

(defvar l-generic-type-predicates
  '((:buffer     . bufferp)
    (:callable   . (lambda (x) (or (functionp x) (subrp x))))
    (:cons       . consp)
    (:float      . floatp)
    (:function   . functionp)
    (:hash-table . hash-table-p)
    (:integer    . integerp)
    (:list       . listp)
    (:null       . null)
    (:number     . numberp)
    (:sequence   . sequencep)
    (:string     . stringp)
    (:symbol     . symbolp)
    (:vector     . vectorp))
  "Mapping of type keywords to predicate functions.

This alist maps type keywords used in pattern matching to their
corresponding predicate functions.  These keywords can be used in
pattern specifications to match arguments based on their type.

Available type keywords:
- :buffer     - matches buffers     (bufferp)
- :callable   - matches functions or subroutines
- :cons       - matches cons cells  (consp)
- :float      - matches floats      (floatp)
- :function   - matches functions   (functionp)
- :hash-table - matches hash tables (hash-table-p)
- :integer    - matches integers    (integerp)
- :list       - matches lists       (listp)
- :null       - matches nil         (null)
- :number     - matches numbers     (numberp)
- :sequence   - matches sequences   (sequencep)
- :string     - matches strings     (stringp)
- :symbol     - matches symbols     (symbolp)
- :vector     - matches vectors     (vectorp)

Example usage in patterns:
  (arg :integer)  ; matches when arg is an integer
  (x :string)     ; matches when x is a string
  (fn :callable)  ; matches when fn is callable")

(provide 'l-generic-type-predicates)
;;; l-generic-type-predicates.el ends here.
