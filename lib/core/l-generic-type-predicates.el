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

(defun l--alistp (obj)
  "Return t if OBJ is an alist (association list).
An alist is a list where every element is a cons cell."
  (and (listp obj)
       (cl-every #'consp obj)))

(defun l--instancep (obj)
  "Return t if OBJ is a struct or EIEIO object instance.
This matches both cl-defstruct instances and EIEIO class instances."
  (or (cl-struct-p obj)
      (eieio-object-p obj)))

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
    (:plist       . plistp)
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
