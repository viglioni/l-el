;;; lmonoid-test.el --- Tests for lmonoid typeclass -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Since: NEXT

;; This file is part of l-el.

;;; Commentary:

;; Tests for the Monoid typeclass.

;;; Code:

(require 'l-test-helpers)
(l-require 'l-typeclasses)

(defun l-test--tree-contains-p (form target)
  "Return non-nil if TARGET appears anywhere in FORM as a subexpression.
Used by macro-expansion tests to assert that a runtime call wasn't
collapsed to a literal at macro-expansion time."
  (or (equal form target)
      (and (consp form)
           (or (l-test--tree-contains-p (car form) target)
               (l-test--tree-contains-p (cdr form) target)))))

(context "lmonoid.el"
  (describe "lmonoid-p predicate"
    (test-it "returns nil for types without monoid instance"
      (expect (lmonoid-p :undefined-monoid-type) :to-be nil)))

  (describe "lmonoid with lists"
    (test-it "lempty returns empty list"
      (expect (lempty :list) :to-equal nil))

    (test-it "lmonoid-p returns t for lists"
      (expect (lmonoid-p :list) :to-be t))

    (test-it "satisfies left identity: empty <> a = a"
      (expect (l<> (lempty :list) '(1 2 3))
              :to-equal '(1 2 3)))

    (test-it "satisfies right identity: a <> empty = a"
      (expect (l<> '(1 2 3) (lempty :list))
              :to-equal '(1 2 3)))

    (test-it "lmappend concatenates all lists"
      (expect (lmappend '(1 2) '(3 4) '(5 6))
              :to-equal '(1 2 3 4 5 6))))

  (describe "lmonoid with strings"
    (test-it "lempty returns empty string"
      (expect (lempty :string) :to-equal ""))

    (test-it "lmonoid-p returns t for strings"
      (expect (lmonoid-p :string) :to-be t))

    (test-it "satisfies left identity: empty <> a = a"
      (expect (l<> (lempty :string) "hello")
              :to-equal "hello"))

    (test-it "satisfies right identity: a <> empty = a"
      (expect (l<> "hello" (lempty :string))
              :to-equal "hello"))

    (test-it "lmappend concatenates all strings"
      (expect (lmappend "hello" " " "world" "!")
              :to-equal "hello world!")))

  (describe "lempty default behavior"
    (test-it "returns error message for non-monoid types"
      (expect (lempty :undefined-monoid-type)
              :to-match "is not an instance of Monoid")))

  ;; ------------------------------------------------------------------
  ;; Bug-catching tests for `lmonoid' macro
  ;; ------------------------------------------------------------------

  (describe "BUG: lsemigroup-p check must be deferred to runtime"
    ;; The macro currently writes `(unless ,(lsemigroup-p name) ...)' —
    ;; the unquote evaluates `lsemigroup-p' at macro-expansion time and
    ;; bakes a literal t/nil into the expansion.  The fix is to drop the
    ;; unquote so the check runs every time the form is evaluated.
    (test-it "the expansion contains a runtime (lsemigroup-p NAME) call"
      (let ((expansion (macroexpand
                        '(lmonoid :test-deferred-check :id 0 :op #'+))))
        (expect (l-test--tree-contains-p
                 expansion '(lsemigroup-p :test-deferred-check))
                :to-be-truthy))))

  (describe "BUG: :concat-fn is applied to the rest list"
    ;; The `:concat-fn' branch of the macro currently writes
    ;; `(funcall ,concat-fn a b)' — but `a' and `b' are not bound by
    ;; the surrounding `(xs :rest ,name)' pattern.  Calling `lmappend'
    ;; on a monoid defined with `:concat-fn' errors with `void-variable
    ;; a' today.  The fix is to apply the function to `xs' (the rest
    ;; list bound by the pattern).
    (before-all
      ;; Register a unique keyword type so the test doesn't interfere
      ;; with the :string / :list / :vector monoids defined globally.
      (unless (assq :l-test-cf-type l-generic-type-predicates)
        (push '(:l-test-cf-type . integerp) l-generic-type-predicates))
      (lmonoid :l-test-cf-type :id 0 :op #'+ :concat-fn #'+))

    (test-it "lmappend applies :concat-fn to the rest list"
      (expect (lmappend 1 2 3) :to-equal 6))

    (test-it "lmappend with a single value applies :concat-fn"
      (expect (lmappend 42) :to-equal 42))))

;;; lmonoid-test.el ends here
