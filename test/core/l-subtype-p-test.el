;;; -*- lexical-binding: t; -*-
;;
;; Tests for hierarchy-walking helpers that back `ldef' dispatch:
;;   - `l-subtype-p'           — keyword subtype relation via `l-type-hierarchy'
;;   - `l--keyword-type-depth' — depth in the keyword hierarchy
;;   - `l--class-cpl-names'    — class precedence list (EIEIO + cl-defstruct)
;;   - `l--cpl-position-of'    — position of a class symbol in a value's CPL

(require 'l-test-helpers)
(require 'l-main)
(require 'cl-lib)
(require 'eieio)

(context "l-subtype-p"

  (describe "identity"
    (test-it ":list is a subtype of itself"
      (expect (l-subtype-p :list :list) :to-be-truthy))
    (test-it ":number is a subtype of itself (category type)"
      (expect (l-subtype-p :number :number) :to-be-truthy)))

  (describe "direct parent"
    (test-it ":list is a subtype of :sequence"
      (expect (l-subtype-p :list :sequence) :to-be-truthy))
    (test-it ":integer is a subtype of :number"
      (expect (l-subtype-p :integer :number) :to-be-truthy))
    (test-it ":vector is a subtype of :array"
      (expect (l-subtype-p :vector :array) :to-be-truthy)))

  (describe "transitive"
    (test-it ":natural is a subtype of :number (via :integer)"
      (expect (l-subtype-p :natural :number) :to-be-truthy))
    (test-it ":alist is a subtype of :sequence (via :list)"
      (expect (l-subtype-p :alist :sequence) :to-be-truthy)))

  (describe "multi-parent"
    (test-it ":vector is a subtype of :sequence"
      (expect (l-subtype-p :vector :sequence) :to-be-truthy))
    (test-it ":vector is a subtype of :array"
      (expect (l-subtype-p :vector :array) :to-be-truthy))
    (test-it ":string is a subtype of :sequence"
      (expect (l-subtype-p :string :sequence) :to-be-truthy))
    (test-it ":string is a subtype of :array"
      (expect (l-subtype-p :string :array) :to-be-truthy)))

  (describe "unrelated"
    (test-it ":list is NOT a subtype of :integer"
      (expect (l-subtype-p :list :integer) :not :to-be-truthy))
    (test-it ":vector is NOT a subtype of :number"
      (expect (l-subtype-p :vector :number) :not :to-be-truthy))
    (test-it ":number is NOT a subtype of :integer (reverse direction)"
      (expect (l-subtype-p :number :integer) :not :to-be-truthy))
    (test-it ":sequence is NOT a subtype of :list (parent vs child)"
      (expect (l-subtype-p :sequence :list) :not :to-be-truthy)))

  (describe "aliases"
    (test-it ":str resolves to :sequence through its parent chain"
      (expect (l-subtype-p :str :sequence) :to-be-truthy))
    (test-it ":int resolves to :number through its parent chain"
      (expect (l-subtype-p :int :number) :to-be-truthy))
    (test-it ":seq is a subtype of :sequence"
      (expect (l-subtype-p :seq :sequence) :to-be-truthy))))


(context "l--type-hierarchy-some"

  (describe "ancestor walk"
    (test-it "finds an ancestor that matches the predicate"
      (expect (l--type-hierarchy-some
               :natural (lambda (kw) (eq kw :number)))
              :to-be-truthy))
    (test-it "returns nil when no ancestor matches"
      (expect (l--type-hierarchy-some
               :natural (lambda (kw) (eq kw :string)))
              :to-be nil))
    (test-it "does NOT test the keyword itself (only ancestors)"
      ;; Catches the contract that makes this safe to call from a
      ;; generic's catch-all without infinite recursion.
      (expect (l--type-hierarchy-some
               :natural (lambda (kw) (eq kw :natural)))
              :to-be nil)))

  (describe "alias chains"
    (test-it "walks through alias parent chains transitively"
      (expect (l--type-hierarchy-some
               :str (lambda (kw) (eq kw :sequence)))
              :to-be-truthy)))

  (describe "category roots"
    (test-it "returns nil for a category type (no parents to walk)"
      (expect (l--type-hierarchy-some
               :number (lambda (_) t))
              :to-be nil))))


(context "l--keyword-type-depth"

  (describe "categories are depth 0"
    (test-it ":number is depth 0 (not a key in hierarchy)"
      (expect (l--keyword-type-depth :number) :to-equal 0))
    (test-it ":sequence is depth 0"
      (expect (l--keyword-type-depth :sequence) :to-equal 0))
    (test-it ":array is depth 0"
      (expect (l--keyword-type-depth :array) :to-equal 0)))

  (describe "primitives one step from a category are depth 1"
    (test-it ":integer is depth 1 (parent :number)"
      (expect (l--keyword-type-depth :integer) :to-equal 1))
    (test-it ":list is depth 1 (parent :sequence)"
      (expect (l--keyword-type-depth :list) :to-equal 1))
    (test-it ":vector is depth 1 (parents :sequence + :array)"
      (expect (l--keyword-type-depth :vector) :to-equal 1))
    (test-it ":string is depth 1 (parents :sequence + :array)"
      (expect (l--keyword-type-depth :string) :to-equal 1)))

  (describe "deeper primitives accumulate depth"
    (test-it ":natural is depth 2 (via :integer)"
      (expect (l--keyword-type-depth :natural) :to-equal 2))
    (test-it ":alist is depth 2 (via :list)"
      (expect (l--keyword-type-depth :alist) :to-equal 2))
    (test-it ":plist is depth 2 (via :list)"
      (expect (l--keyword-type-depth :plist) :to-equal 2))))


(context "class CPL helpers"

  ;; Hierarchy shared by all the CPL tests:
  ;;
  ;;        cpl-food
  ;;       /        \
  ;;   cpl-fruit  cpl-sweet
  ;;       \        /
  ;;    cpl-candy-apple
  ;;
  ;; Plus a single-inheritance cl-defstruct chain for parity:
  ;;   sfood -> sfruit -> sapple
  (before-all
    (defclass cpl-food  ()                          ())
    (defclass cpl-fruit (cpl-food)                  ())
    (defclass cpl-sweet (cpl-food)                  ())
    (defclass cpl-candy-apple (cpl-fruit cpl-sweet) ())
    (cl-defstruct cpl-sfood)
    (cl-defstruct (cpl-sfruit (:include cpl-sfood)))
    (cl-defstruct (cpl-sapple (:include cpl-sfruit))))

  (describe "l--class-cpl-names for EIEIO classes"
    (test-it "single-class chain starts with the class itself"
      (let ((cpl (l--class-cpl-names 'cpl-fruit)))
        (expect (car cpl) :to-equal 'cpl-fruit)
        (expect (memq 'cpl-food cpl) :to-be-truthy)))
    (test-it "diamond child lists C3-linearized ancestors"
      (let ((cpl (l--class-cpl-names 'cpl-candy-apple)))
        (expect (car cpl) :to-equal 'cpl-candy-apple)
        ;; defclass parent order is (cpl-fruit cpl-sweet), so C3 puts
        ;; cpl-fruit before cpl-sweet.
        (expect (cl-position 'cpl-fruit cpl)
                :to-be-less-than
                (cl-position 'cpl-sweet cpl))
        ;; The shared root appears after both parents.
        (expect (cl-position 'cpl-food cpl)
                :to-be-greater-than
                (cl-position 'cpl-sweet cpl)))))

  (describe "l--class-cpl-names for cl-defstruct"
    (test-it "chain starts with the struct itself"
      (let ((cpl (l--class-cpl-names 'cpl-sapple)))
        (expect (car cpl) :to-equal 'cpl-sapple)))
    (test-it "includes the :include parents in order"
      (let ((cpl (l--class-cpl-names 'cpl-sapple)))
        (expect (cl-position 'cpl-sapple cpl)
                :to-be-less-than
                (cl-position 'cpl-sfruit cpl))
        (expect (cl-position 'cpl-sfruit cpl)
                :to-be-less-than
                (cl-position 'cpl-sfood cpl)))))

  (describe "l--class-cpl-names for unknown symbols"
    (test-it "returns nil for a symbol with no class metadata"
      (expect (l--class-cpl-names 'no-such-class-at-all) :to-equal nil)))

  (describe "l--class-cpl-names for built-in type names"
    ;; Locks in the version-dependent observable contract.  On Emacs 30
    ;; `cl-find-class' returns a `built-in-class' metaobject for type
    ;; names like `integer'; that object inherits from the same
    ;; `cl--class' parent struct that exposes `cl--class-parents', so it
    ;; falls through the cl-defstruct branch of `l--class-cpl-names' and
    ;; yields a real CPL.  On Emacs < 30 the same call returns nil
    ;; because built-in types have no metaobject.
    (test-it "Emacs >= 30 returns a CPL whose head is the type itself"
      ;; Skipped on older Emacs where built-ins have no metaobject.
      (when (>= emacs-major-version 30)
        (let ((cpl (l--class-cpl-names 'integer)))
          (expect cpl :to-be-truthy)
          (expect (car cpl) :to-equal 'integer))))
    (test-it "Emacs < 30 returns nil for built-in type names"
      (when (< emacs-major-version 30)
        (expect (l--class-cpl-names 'integer) :to-equal nil))))

  (describe "l--cpl-position-of"
    (test-it "returns 0 for the value's own class"
      (let ((a (make-instance 'cpl-fruit)))
        (expect (l--cpl-position-of a 'cpl-fruit) :to-equal 0)))
    (test-it "returns a higher index for ancestors"
      (let ((a (make-instance 'cpl-fruit)))
        (expect (l--cpl-position-of a 'cpl-food) :to-be-greater-than 0)))
    (test-it "returns nil when the class is not an ancestor"
      (let ((a (make-instance 'cpl-fruit)))
        (expect (l--cpl-position-of a 'cpl-sweet) :to-equal nil)))
    (test-it "diamond child: closer parent has a lower CPL position"
      (let ((c (make-instance 'cpl-candy-apple)))
        (expect (l--cpl-position-of c 'cpl-candy-apple)
                :to-be-less-than
                (l--cpl-position-of c 'cpl-fruit))
        (expect (l--cpl-position-of c 'cpl-fruit)
                :to-be-less-than
                (l--cpl-position-of c 'cpl-food))))
    (test-it "works for cl-defstruct instances too"
      (let ((s (make-cpl-sapple)))
        (expect (l--cpl-position-of s 'cpl-sapple) :to-equal 0)
        (expect (l--cpl-position-of s 'cpl-sfruit) :to-be-greater-than 0)))))

;;; l-subtype-p-test.el ends here
