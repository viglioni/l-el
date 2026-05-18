;;; algebra.el --- Algebraic structures (Semigroup, Monoid, etc.) -*- lexical-binding: t; l-syntax: t; -*-

;; Copyright (C) 2025
;; since: NEXT
;; updated-at: ()

;; This file is part of l-el.

;;; Commentary:

;; Algebraic structure typeclasses for l-el
;;
;; This file contains the algebraic hierarchy:
;;   Semigroup -> Monoid -> Group -> Ring -> Field
;;
;; Currently implemented:
;; - Semigroup: Associative binary operation
;; - Monoid: Semigroup with identity element

;;; Code:

(require 'l-main)

;;
;; Semigroup
;;

@doc "Check if a type/struct/class is an instance of `lsemigroup'.

since: NEXT"
(ldef lsemigroup-p type-keyword
      -> (l--type-hierarchy-some type-keyword #'lsemigroup-p))

@doc "Calculate the a <> b, where a and b are instances of `lsemigroup'.

since: NEXT"
(ldef l<> any _   -> (format "%s is not an instance of SemiGroup" any))
(ldef l<> _   any -> (format "%s is not an instance of SemiGroup" any))

@doc "Return the binary operator backing the lsemigroup instance for TYPE.

since: NEXT"
(ldef l<>-info any -> (format "%s is not an instance of SemiGroup" any))

(cl-defmacro lsemigroup (name &key op)
  "Define a Semigroup instance for type NAME with binary operation OP.

A Semigroup is an algebraic structure consisting of a set together with an
associative binary operation (<>). The key property is associativity:
  (a <> b) <> c = a <> (b <> c)

Arguments:
  NAME - A keyword symbol identifying the type (e.g., :string, :list)
  OP   - A binary function or value for the associative operation

The macro defines:
  - (l<> a b) -> result        ; Combine two values
  - (l<> NAME) -> OP           ; Get the operation
  - (lsemigroup-p NAME) -> t   ; Type predicate

Example:
  (lsemigroup :string :op #'concat)
  (l<> \"hello\" \" world\")  ; => \"hello world\"

The operation OP must be associative:
  (a <> b) <> c = a <> (b <> c)

Common semigroups:
  - Strings:  (lsemigroup :string :op #'concat)
  - Lists:    (lsemigroup :list :op #'append)
  - Vectors:  (lsemigroup :vector :op #'vconcat)

since: NEXT"
  `(progn
     ;; Operator
     (ldef l<> (a ,name) (b ,name) -> (if (functionp ,op)
                                          (funcall ,op a b)
                                        ,op))
     ;; Meta
     (ldef lsemigroup-p ,name     -> t)
     (ldef lsemigroup-p (_ ,name) -> t)
     (ldef l<>-info     ,name     -> ,op)))

;;
;; Monoid
;;

@doc "Check if a type/struct/class is an instance of `lmonoid'.

since: NEXT"
(ldef lmonoid-p type-keyword
      -> (l--type-hierarchy-some type-keyword #'lmonoid-p))

@doc "Return the identity element of the instance of `lmonoid'.

since: NEXT"
(ldef lempty    any -> (format "%s is not an instance of Monoid" any))

@doc "Calculate the binary operator `l<>' over several items instances of `lmonoid'.

since: NEXT"
(ldef lmappend  any -> (format "%s is not an instance of Monoid" any))

(cl-defmacro lmonoid (name &key id op concat-fn)
  "Define a Monoid instance for type NAME with identity ID and operation OP.

A Monoid is a Semigroup with an identity element (also called empty or mempty).
It satisfies two additional laws:
  - Left identity:  empty <> a = a
  - Right identity: a <> empty = a
  - Associativity:  (a <> b) <> c = a <> (b <> c) (from Semigroup)

Arguments:
  NAME       - A keyword symbol identifying the type (e.g., :list, :string)
  ID         - The identity/empty element for this monoid
  OP         - A binary function for the associative operation
  CONCAT-FN  - Optional custom function to concatenate multiple values

The macro defines:
  - (lempty NAME) -> ID                   ; Get identity element
  - (lmappend xs...) -> result            ; Fold over values with <>
  - (lmonoid-p NAME) -> t                 ; Type predicate
  - Automatically creates the Semigroup instance if needed

Example:
  (lmonoid :string :id \"\" :op #'concat)
  (lempty :string)              ; => \"\"
  (l<> \"hello\" \" world\")    ; => \"hello world\"
  (lmappend \"a\" \"b\" \"c\")  ; => \"abc\"

Common monoids:
  - Strings:  (lmonoid :string :id \"\" :op #'concat)
  - Lists:    (lmonoid :list :id nil :op #'append)
  - Vectors:  (lmonoid :vector :id [] :op #'vconcat)

since: NEXT"
  `(progn
     (unless (lsemigroup-p ,name)
       (lsemigroup ,name :op ,op))
     ;; Identity
     (ldef lempty ,name     -> ,id)
     (ldef lempty (_ ,name) -> ,id)
     ;; mconcat
     (if ,concat-fn
         (ldef lmappend (xs :rest ,name) -> (if (functionp ,concat-fn)
                                                (apply ,concat-fn xs)
                                              ,concat-fn))
       (ldef lmappend (xs :rest ,name) ->
             (cl-reduce #'l<> xs :initial-value (lempty ,name))))
     ;; Meta
     (ldef lmonoid-p ,name -> t)))



(provide 'algebra)

;;; algebra.el ends here
