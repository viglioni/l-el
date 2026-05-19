;;; l-algebra-test.el --- Tests for algebraic typeclasses -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Since: NEXT

;; This file is part of l-el.

;;; Commentary:

;; Tests for the algebraic typeclasses (Semigroup, Monoid, etc.)

;;; Code:

(require 'l-test-helpers)
(l-require 'l-typeclasses)

(context "l-algebra.el"
  (describe "lsemigroup-p predicate"
    (test-it "returns nil for types without semigroup instance"
      (expect (lsemigroup-p :undefined-type) :to-be nil)))

  (describe "lsemigroup with strings"
    (test-it "concatenates strings"
      (expect (l<> "hello" (l<> " " "world")) :to-equal "hello world"))

    (test-it "lsemigroup-p returns t for strings"
      (expect (lsemigroup-p :string) :to-be t))

    (test-it "l<>-info returns the operator for a type keyword"
      (expect (l<>-info :string) :to-equal #'concat))

    (test-it "l<> with a single non-keyword argument currys"
      (expect (functionp (l<> "asd")) :to-be-truthy)
      (expect (funcall (l<> "asd") "dsa") :to-equal "asddsa"))

    (test-it "satisfies associativity with strings"
      (dolist (triple '(("foo" "bar" "baz")
                        (""    "x"   "yz")
                        ("a "  "b "  "c")))
        (pcase-let ((`(,a ,b ,c) triple))
          (expect (l<> (l<> a b) c)
                  :to-equal
                  (l<> a (l<> b c)))))))

  (describe "lsemigroup with lists"
    (test-it "concatenates lists"
      (expect (l<> '(1 2) '(3 4)) :to-equal '(1 2 3 4)))

    (test-it "lsemigroup-p returns t for lists"
      (expect (lsemigroup-p :list) :to-be t))

    (test-it "satisfies associativity with lists"
      (dolist (triple '(((1)     (2)       (3))
                        (nil     (4 5)     (6))
                        ((1 2 3) (4 5 6)   (7 8))))
        (pcase-let ((`(,a ,b ,c) triple))
          (expect (l<> (l<> a b) c)
                  :to-equal
                  (l<> a (l<> b c)))))))

  (describe "lsemigroup with vectors"
    (test-it "concatenates vectors"
      (expect (l<> [1 2] [3 4]) :to-equal [1 2 3 4]))

    (test-it "lsemigroup-p returns t for vectors"
      (expect (lsemigroup-p :vector) :to-be t))

    (test-it "l<>-info returns the operator for :vector"
      (expect (l<>-info :vector) :to-equal #'vconcat))

    (test-it "satisfies associativity with vectors"
      (dolist (triple '(([1]     [2]       [3])
                        ([]      [4 5]     [6])
                        ([1 2 3] [4 5 6]   [7 8])))
        (pcase-let ((`(,a ,b ,c) triple))
          (expect (l<> (l<> a b) c)
                  :to-equal
                  (l<> a (l<> b c)))))))

  (describe "l<>-info default behavior"
    (test-it "returns error message for non-semigroup types"
      (expect (l<>-info :undefined-type)
              :to-match "is not an instance of SemiGroup")))

  (describe "lsemigroup-p with type hierarchy"
    (before-all
      ;; Define a semigroup for :sequence
      (lsemigroup :sequence :op #'append))

    (test-it "returns t for parent type :sequence"
      (expect (lsemigroup-p :sequence) :to-be t))

    (test-it "returns t for child type :list (parent is :sequence)"
      (expect (lsemigroup-p :list) :to-be t))

    (test-it "returns t for child type :vector (parent is :sequence)"
      (expect (lsemigroup-p :vector) :to-be t))

    (test-it "returns nil for types without semigroup instance in hierarchy"
      (expect (lsemigroup-p :integer) :to-be nil))))

;;; l-algebra-test.el ends here
