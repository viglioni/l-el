;;; l-generic-type-predicates.el --- Functions for type match pattern -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Laura Viglioni

;; Author: Laura Viglioni
;; Keywords: lisp, functional, programming, generics, pattern-matching
;; URL: https://github.com/viglioni/l-el
;; since: 0.2.0
;; updated-at: (0.3.0 0.5.0 1.0.0 1.1.0 1.1.1)

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

(defun l--naturalp (obj)
  "Return if obj is a natural number {1, 2, 3, ...}.

since: 1.1.1"
  (and (integerp obj)
       (> obj 0)))

(defun l--alistp (obj)
  "Return t if OBJ is an alist (association list).
An alist is a list where every element is a cons cell.

since: 0.5.0"
  (and (listp obj)
       (cl-every #'consp obj)))

(defun l--plistp (obj)
  "Return t if OBJ is a plist (property list).
A plist is a list with an even number of elements.

This is a compatibility wrapper for `plistp', which was added in Emacs 29.
Uses the built-in `plistp' if available, otherwise provides a fallback
implementation for Emacs < 29.

since: 0.5.0"
  (if (fboundp 'plistp)
      (plistp obj)
    ;; Fallback for Emacs < 29
    (and (listp obj)
         (zerop (mod (length obj) 2)))))

(defun l--instancep (obj)
  "Return t if OBJ is a struct or EIEIO object instance.
This matches both cl-defstruct instances and EIEIO class instances.

since: 0.5.0"
  (or (cl-struct-p obj)
      (eieio-object-p obj)))

(defun l--class-cpl-names (class-symbol)
  "Return CLASS-SYMBOL's class precedence list as a list of name symbols.

Most-specific-first.  Works for EIEIO classes (using C3 linearization
via `eieio--class-precedence-list') and for cl-defstruct types (walking
the single-inheritance chain through `:include').  Returns nil if
CLASS-SYMBOL has no class metadata (built-in types like `integer'
have no class object accessible via `cl-find-class').

since: NEXT"
  (let ((class (cl-find-class class-symbol)))
    (cond
     ((null class) nil)
     ((eieio--class-p class)
      (mapcar #'eieio-class-name (eieio--class-precedence-list class)))
     (t (l--struct-cpl-walk class)))))

(defun l--struct-cpl-walk (class)
  "Recursively walk CLASS and its `cl--class-parents' chain.
Return a flat list of class name symbols, most-specific first.

since: NEXT"
  (when class
    (cons (cl--class-name class)
          (cl-mapcan #'l--struct-cpl-walk (cl--class-parents class)))))

(defun l--cpl-position-of (value class-symbol)
  "Return position of CLASS-SYMBOL in VALUE's class precedence list.

Returns the integer index (0 = most specific) of CLASS-SYMBOL in the
class-precedence list of VALUE's class.  Returns nil if VALUE has no
class metadata or if CLASS-SYMBOL is not an ancestor.

Used by `ldef' dispatch to resolve specificity between class-typed
methods at runtime, including under multiple inheritance.

since: NEXT"
  (let ((cpl (l--class-cpl-names (type-of value))))
    (when cpl
      (cl-position class-symbol cpl))))

(defun l--list-of-p (obj type-or-keyword)
  "Return t if OBJ is a list where every element matches TYPE-OR-KEYWORD.

TYPE-OR-KEYWORD can be:
- A keyword from `l-generic-type-predicates' (e.g., :integer, :string)
- A symbol representing a struct or class type (e.g., point, person)

Examples:
  (l--list-of-p '(1 2 3) :integer)     ; => t
  (l--list-of-p '(1 2 \"3\") :integer) ; => nil
  (l--list-of-p '(\"a\" \"b\") :string) ; => t
  (l--list-of-p '() :integer)          ; => t (empty list matches any type)

  (cl-defstruct point x y)
  (l--list-of-p (list (make-point :x 1 :y 2)) 'point) ; => t

since: 1.1.0"
  (and (listp obj)
       (if (keywordp type-or-keyword)
           ;; Keyword type - use our predicates
           (let ((predicate (cdr (assoc type-or-keyword l-generic-type-predicates))))
             (if predicate
                 (cl-every predicate obj)
               (l--raise-unknown-type-predicate type-or-keyword "list_of validation")))
         ;; Symbol type - use cl-typep for struct/class matching
         (cl-every (lambda (elem) (cl-typep elem type-or-keyword)) obj))))

(defun l--list-of-instances-p (obj type-name)
  "Return t if OBJ is a list where every element is an instance of TYPE-NAME.

TYPE-NAME should be a struct or class type name (not a keyword).

Examples:
  (cl-defstruct point x y)
  (l--list-of-instances-p (list (make-point :x 1 :y 2)) 'point) ; => t
  (l--list-of-instances-p '(1 2 3) 'point)                       ; => nil
  (l--list-of-instances-p '() 'point)                            ; => t (empty list)

since: 0.5.0"
  (and (listp obj)
       (cl-every (lambda (elem) (cl-typep elem type-name)) obj)))

;;;;;;;;;;;;;;;;;;;;;;
;; Type Hierarchy   ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar l--type-hierarchy
  '(;; Sequence types
    (:list        . (:sequence))
    (:vector      . (:sequence :array))
    (:string      . (:sequence :array))
    ;; Number types
    (:integer     . (:number))
    (:float       . (:number))
    (:natural     . (:integer :number))
    ;; Array types (not :sequence)
    (:bool-vector . (:array))
    (:char-table  . (:array))
    ;; Struct and object types
    (:struct      . (:instance))
    (:object      . (:instance))
    ;; Cons types
    (:alist       . (:list :sequence))
    (:plist       . (:list :sequence))
    (:cons        . (:list :sequence))
    ;; Aliases point to their canonical types
    (:seq         . (:sequence))
    (:str         . (:string :sequence :array))
    (:int         . (:integer :number))
    (:nil         . (:list :sequence))
    (:fn          . (:function :callable))
    (:buff        . (:buffer))
    (:bvector     . (:bool-vector :array))
    (:ctable      . (:char-table :array)))
  "Explicit type hierarchy mapping children to their parent types.

Each entry is (CHILD . (PARENT1 PARENT2 ...)) where CHILD is a specific
type and PARENT1, PARENT2, etc. are more general types that include CHILD.

This hierarchy drives subtype relationships for `ldef' dispatch and
typeclass instances.  For example, since :list has :sequence as a
parent, a method declared for :sequence also matches a list, and the
deeper-in-hierarchy method wins when both are defined.

The hierarchy follows these principles:
- Primitive types (e.g., :list, :integer) are children of category types
- Category types (e.g., :sequence, :number) group related primitive types
- Some types have multiple parents (e.g., :vector is both :sequence and :array)
- Aliases resolve to their canonical types with their full parent chain

Examples:
  :list -> :sequence (lists are sequences)
  :vector -> :sequence, :array (vectors are both sequences and arrays)
  :integer -> :number (integers are numbers)
  :natural -> :integer, :number (natural numbers are integers and numbers)
  :str -> :string, :sequence, :array (alias with full parent chain)

Internal: not a stable public API — the shape and contents of this
table may change between releases.

since: 1.1.1")

(defun l--type-hierarchy-some (type-keyword pred)
  "Return non-nil if PRED is satisfied by some ancestor of TYPE-KEYWORD.

Walks TYPE-KEYWORD's parents in `l--type-hierarchy' recursively and
returns the first non-nil value PRED produces.  TYPE-KEYWORD itself
is NOT tested by PRED — this lets PRED be a generic function whose
catch-all calls back into this primitive without infinite recursion.

PRED is a function or symbol naming a function, called with one
keyword argument.

Used by typeclass predicates such as `lsemigroup-p' and `lfunctorp'
to answer \"is this type an instance, transitively, via its parents?\"

since: NEXT"
  (when (keywordp type-keyword)
    (let ((parents (cdr (assq type-keyword l--type-hierarchy))))
      (cl-some (lambda (p)
                 (or (funcall pred p)
                     (l--type-hierarchy-some p pred)))
               parents))))

(defun l--keyword-type-depth (keyword)
  "Return the depth of KEYWORD in `l--type-hierarchy'.

Depth is defined recursively:
  - 0 if KEYWORD is a category (not present as a key in the hierarchy).
  - 1 + max(depth of parents) otherwise.

So `:number' is depth 0 (category), `:integer' is depth 1 (one step
from :number), and `:natural' is depth 2 (two steps from :number via
:integer).  Used as a sub-rank within the primitive-keyword tier so
deeper keywords win specificity ties against shallower ones.

since: NEXT"
  (let ((parents (cdr (assq keyword l--type-hierarchy))))
    (if (null parents)
        0
      (1+ (apply #'max (mapcar #'l--keyword-type-depth parents))))))

(defun l-subtype-p (child parent)
  "Check if CHILD type is a subtype of PARENT type.

Returns t if:
- CHILD is identical to PARENT
- CHILD has PARENT in its direct parent list
- CHILD has an ancestor that is a subtype of PARENT (transitive)

Both CHILD and PARENT should be type keywords (e.g., :list, :sequence).

Examples:
  (l-subtype-p :list :sequence)    ; => t (direct parent)
  (l-subtype-p :natural :number)   ; => t (transitive: :natural -> :integer -> :number)
  (l-subtype-p :list :list)        ; => t (identical)
  (l-subtype-p :list :integer)     ; => nil (unrelated types)
  (l-subtype-p :vector :sequence)  ; => t (vector is a sequence)
  (l-subtype-p :vector :array)     ; => t (vector is also an array)

since: 1.1.1"
  (or (eq child parent)
      (l--type-hierarchy-some child (lambda (kw) (eq kw parent)))))

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

Returns t if ELEMENT matches TYPE, nil otherwise.

since: 1.0.0"
  (if (keywordp type)
      ;; Type is a keyword - check in our predicates registry
      (let ((predicate (cdr (assoc type l-generic-type-predicates))))
        (if predicate
            (funcall predicate element)
          (l--raise-unknown-type-predicate type "type dispatch")))
    ;; Type is not a keyword - use cl-typep for struct/class types
    (cl-typep element type)))

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
instance of a category) but less specific than value matches.

since: 0.5.0")

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
    (:natural     . l--naturalp)
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
    (:integer     . integerp)
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
  (bv :bvector)   ; matches when bv is a bool-vector

since: 0.2.0")

(provide 'l-generic-type-predicates)
;;; l-generic-type-predicates.el ends here.
