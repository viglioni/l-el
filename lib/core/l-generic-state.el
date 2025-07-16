;;; l-generic-state.el --- Functions for type match pattern -*- lexical-binding: t; -*-

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
;; This module provides the hashtable used to implement the generic functions.

;;; Code:

(require 'cl-lib)

(defvar l-generic-method-registry (make-hash-table :test 'equal)
  "Registry of generic function methods.

This hash table stores all registered methods for generic functions.
The structure is: fname -> list of method specifications.

Each method specification is a list containing:
- specificity: numeric score indicating how specific the pattern is
- arity: number of arguments the method accepts
- pattern-list: list of patterns for matching arguments
- body: list of expressions forming the method body

Methods are automatically sorted by specificity (highest first) to ensure
the most specific patterns are matched before more general ones.

Example registry entry:
  \='my-func -> ((1100 2 ((x :integer) (y :string)) (body...))
               (200 2 ((x :number) y) (body...))
               (1 2 (_ _) (body...)))")

(defun l--add-to-registry (name methods)
  "Add item to `l-generic-method-registry'.
Key NAME and value METHODS."
  (puthash name methods l-generic-method-registry))

(defun l--get-from-registry (name)
  "Get item key NAME from `l-generic-method-registry'.
Defaults to empty list."
  (gethash name l-generic-method-registry '()))

(defvar l-generic-doc-registry (make-hash-table :test 'equal)
  "Registry of general documentation of `ldef' functions.
Check `l-doc' for usage.")

(defun l--add-doc-registry (fname docstring)
  "Add item to `l-generic-doc-registry'.
Key FNAME and value DOCSTRING."
  (puthash fname docstring l-generic-doc-registry))

(cl-defmethod l--get-doc-registry ((fname symbol))
  "Get item key FNAME from `l-generic-doc-registry'.
Defaults to \"Not documented\"."
  (gethash fname l-generic-doc-registry "Not documented."))


(cl-defstruct l-generic-method-spec
  "Struct to fill `l-generic-method-registry' with methods' information.
Each method spec contains:
- `arity': Number of arguments (for dispatch optimization)
- `body': Code to execute when matched
- `pattern-list': The actual patterns to match
- `specificity': Numeric score for pattern matching priority"
  specificity arity pattern-list body)


(defun l--method (arity body pattern-list specificity)
  "Create \='l-generic-method-spec.
ARITY BODY PATTERN-LIST SPECIFICITY are the struct params."
  (make-l-generic-method-spec
   :arity         arity
   :body          body
   :pattern-list  pattern-list
   :specificity   specificity))

(cl-defmethod l--arity ((m l-generic-method-spec))
  "Get the arity from method specification M."
  (l-generic-method-spec-arity m))
(cl-defmethod l--body ((m l-generic-method-spec))
  "Get the body from method specification M."
  (l-generic-method-spec-body m))
(cl-defmethod l--pattern-list ((m l-generic-method-spec))
  "Get the pattern list from method specification M."
  (l-generic-method-spec-pattern-list m))
(cl-defmethod l--specificity ((m l-generic-method-spec))
  "Get the specificity from method specification M."
  (l-generic-method-spec-specificity m))

(provide 'l-generic-state)
;;; l-generic-state.el ends here
