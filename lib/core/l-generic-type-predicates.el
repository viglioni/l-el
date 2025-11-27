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

(require 'cl-lib)
(require 'eieio)
(require 'l-exception)

(defun l--alistp (obj)
  "Return t if OBJ is an alist (association list).
An alist is a list where every element is a cons cell."
  (and (listp obj)
       (cl-every #'consp obj)))

(defun l--plistp (obj)
  "Return t if OBJ is a plist (property list).
A plist is a list with an even number of elements.

This is a compatibility wrapper for `plistp', which was added in Emacs 29.
Uses the built-in `plistp' if available, otherwise provides a fallback
implementation for Emacs < 29."
  (if (fboundp 'plistp)
      (plistp obj)
    ;; Fallback for Emacs < 29
    (and (listp obj)
         (zerop (mod (length obj) 2)))))

(defun l--instancep (obj)
  "Return t if OBJ is a struct or EIEIO object instance.
This matches both cl-defstruct instances and EIEIO class instances."
  (or (cl-struct-p obj)
      (eieio-object-p obj)))

(defun l--list-of-p (obj type-keyword)
  "Return t if OBJ is a list where every element matches TYPE-KEYWORD.

TYPE-KEYWORD should be a keyword from `l-generic-type-predicates'.

Examples:
  (l--list-of-p '(1 2 3) :integer)     ; => t
  (l--list-of-p '(1 2 \"3\") :integer) ; => nil
  (l--list-of-p '(\"a\" \"b\") :string) ; => t
  (l--list-of-p '() :integer)          ; => t (empty list matches any type)"
  (and (listp obj)
       (let ((predicate (cdr (assoc type-keyword l-generic-type-predicates))))
         (if predicate
             (cl-every predicate obj)
           (l--raise-unknown-type-predicate type-keyword "list_of validation")))))

(defun l--list-of-instances-p (obj type-name)
  "Return t if OBJ is a list where every element is an instance of TYPE-NAME.

TYPE-NAME should be a struct or class type name (not a keyword).

Examples:
  (cl-defstruct point x y)
  (l--list-of-instances-p (list (make-point :x 1 :y 2)) 'point) ; => t
  (l--list-of-instances-p '(1 2 3) 'point)                       ; => nil
  (l--list-of-instances-p '() 'point)                            ; => t (empty list)"
  (and (listp obj)
       (cl-every (lambda (elem) (cl-typep elem type-name)) obj)))

(defun l-instanceof (element type)
  "Check if ELEMENT is an instance of TYPE.

TYPE can be either:
- A keyword from `l-generic-type-predicates' (e.g., :integer, :string, :list)
- A struct or class type name for use with `cl-typep' (e.g., 'point, 'my-struct)

Examples:
  (l-instanceof 42 :integer)           ; => t
  (l-instanceof \"hello\" :string)     ; => t
  (l-instanceof '(1 2 3) :list)        ; => t

  (cl-defstruct point x y)
  (l-instanceof (make-point) 'point)   ; => t
  (l-instanceof 42 'point)             ; => nil

Returns t if ELEMENT matches TYPE, nil otherwise."
  (if (keywordp type)
      ;; Type is a keyword - check in our predicates registry
      (let ((predicate (cdr (assoc type l-generic-type-predicates))))
        (if predicate
            (funcall predicate element)
          (error "Unknown type keyword: %s" type)))
    ;; Type is not a keyword - use cl-typep for struct/class types
    (cl-typep element type)))

(defvar l-generic-primitive-types
  '(:alist :bool-vector :buffer :char-table :cons :float :function
    :hash-table :integer :list :null :object :plist :record :string
    :struct :symbol :vector
    ;; Aliases
    :buff :bvector :ctable :fn :int :nil :str)
  "Primitive/specific types that should match before category types.
These types are concrete and should have higher specificity than
category types like :sequence, :array, :number, etc.")

(defvar l-generic-category-types
  '(:array :callable :instance :number :sequence
    ;; Aliases
    :seq)
  "Category/composite types that match multiple primitive types.
These have lower specificity than primitive types and should only
match when no primitive type matches.")

(defvar l-generic-parameterized-type-predicates
  '((:instance_of        . cl-typep)
    (:list_of            . l--list-of-p)
    (:list_of_instances  . l--list-of-instances-p))
  "Type predicates that require an additional type argument.

These predicates take both a value and a type specifier as arguments.

Available parameterized types:

- :instance_of - uses cl-typep to check if a value is an instance of a
  specific struct or class type.
  Usage: (arg :instance_of type-name)
  Example: (ldef process-point (p :instance_of point) -> ...)

- :list_of - uses l--list-of-p to check if a value is a list where every
  element matches a specific type keyword.
  Usage: (arg :list_of :type-keyword)
  Example: (ldef sum-integers (nums :list_of :integer) -> ...)

- :list_of_instances - uses l--list-of-instances-p to check if a value is
  a list where every element is an instance of a specific struct/class type.
  Usage: (arg :list_of_instances type-name)
  Example: (ldef process-points (pts :list_of_instances point) -> ...)

These are more specific than regular type predicates (which match any
instance of a category) but less specific than value matches.")

(defvar l-generic-type-predicates
  '(;; Primitive/specific types
    (:alist       . l--alistp)
    (:bool-vector . bool-vector-p)
    (:buffer      . bufferp)
    (:char-table  . char-table-p)
    (:cons        . consp)
    (:float       . floatp)
    (:function    . functionp)
    (:hash-table  . hash-table-p)
    (:integer     . integerp)
    (:list        . listp)
    (:null        . null)
    (:object      . eieio-object-p)
    (:plist       . l--plistp)
    (:record      . recordp)
    (:string      . stringp)
    (:struct      . cl-struct-p)
    (:symbol      . symbolp)
    (:vector      . vectorp)
    ;; Composite/category types
    (:array       . arrayp)
    (:callable    . (lambda (x) (or (functionp x) (subrp x))))
    (:instance    . l--instancep)
    (:number      . numberp)
    (:sequence    . sequencep)
    ;; Aliases (short forms)
    (:buff        . bufferp)
    (:bvector     . bool-vector-p)
    (:ctable      . char-table-p)
    (:fn          . functionp)
    (:int         . integerp)
    (:nil         . null)
    (:seq         . sequencep)
    (:str         . stringp))
  "Mapping of type keywords to predicate functions.

This alist maps type keywords used in pattern matching to their
corresponding predicate functions.  These keywords can be used in
pattern specifications to match arguments based on their type.

Available type keywords:

Primitive/specific types:
- :alist       - matches alists       (l--alistp) - association lists ((k . v) ...)
- :bool-vector - matches bool-vectors (bool-vector-p) - compact bit arrays
- :buffer      - matches buffers      (bufferp)
- :char-table  - matches char-tables  (char-table-p) - character lookup tables
- :cons        - matches cons cells   (consp)
- :float       - matches floats       (floatp)
- :function    - matches functions    (functionp)
- :hash-table  - matches hash tables  (hash-table-p)
- :integer     - matches integers     (integerp)
- :list        - matches lists        (listp)
- :null        - matches nil          (null)
- :object      - matches EIEIO objects (eieio-object-p) - class instances
- :plist       - matches plists       (plistp) - property lists (:k v ...)
- :record      - matches records      (recordp) - generic record type
- :string      - matches strings      (stringp)
- :struct      - matches structs      (cl-struct-p) - cl-defstruct instances
- :symbol      - matches symbols      (symbolp)
- :vector      - matches vectors      (vectorp)

Composite/category types:
- :array       - matches arrays       (arrayp) - vectors, strings, char-tables, bool-vectors
- :callable    - matches functions or subroutines
- :instance    - matches instances    (l--instancep) - cl-defstruct and EIEIO objects
- :number      - matches numbers      (numberp) - integers and floats
- :sequence    - matches sequences    (sequencep) - lists, vectors, strings

Aliases (short forms):
- :buff        - alias for :buffer
- :bvector     - alias for :bool-vector
- :ctable      - alias for :char-table
- :fn          - alias for :function
- :int         - alias for :integer
- :nil         - alias for :null
- :seq         - alias for :sequence
- :str         - alias for :string

Example usage in patterns:
  (arg :integer)  ; matches when arg is an integer
  (arg :int)      ; same as above (alias)
  (x :string)     ; matches when x is a string
  (x :str)        ; same as above (alias)
  (fn :callable)  ; matches when fn is callable
  (bv :bvector)   ; matches when bv is a bool-vector")

(provide 'l-generic-type-predicates)
;;; l-generic-type-predicates.el ends here.
