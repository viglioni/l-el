;;; -*- lexical-binding: t; -*-
;;
;; Per-type dispatch coverage for `ldef'.
;;
;; One positive match per supported type, plus tricky-negative cases drawn
;; from types that overlap (e.g. -1 / 0 / 2.4 against :natural; vector
;; against :list).  Aliases get one smoke test confirming they match
;; their canonical's values.
;;
;; Specificity ordering (primitive-beats-category, etc.) is exercised in
;; `ldef-types-test.el', not here.

(require 'l-test-helpers)
(require 'l-main)
(require 'cl-lib)
(require 'eieio)

;; Shared fixtures.
(cl-defstruct l-test-coverage-point x y)
(defclass l-test-coverage-eieio ()
  ((slot :initarg :slot :initform nil)))

(context "ldef per-type dispatch coverage"

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Primitive types
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (describe ":alist"
    (before-all
      (ldef tt-alist (x :alist) -> :alist)
      (ldef tt-alist _ -> :other))
    (test-it "matches alist of cons pairs"
      (expect (tt-alist '((a . 1) (b . 2))) :to-equal :alist))
    (test-it "rejects plist (similar shape, flat key/value)"
      (expect (tt-alist '(:a 1 :b 2)) :to-equal :other))
    (test-it "rejects flat list"
      (expect (tt-alist '(1 2 3)) :to-equal :other)))

  (describe ":bool-vector"
    (before-all
      (ldef tt-bv (x :bool-vector) -> :bv)
      (ldef tt-bv _ -> :other))
    (test-it "matches bool-vector"
      (expect (tt-bv (make-bool-vector 3 t)) :to-equal :bv))
    (test-it "rejects regular vector (also an :array)"
      (expect (tt-bv [t nil t]) :to-equal :other)))

  (describe ":buffer"
    (before-all
      (ldef tt-buf (x :buffer) -> :buf)
      (ldef tt-buf _ -> :other))
    (test-it "matches a buffer object"
      (let ((b (get-buffer-create "*l-type-coverage*")))
        (unwind-protect
            (expect (tt-buf b) :to-equal :buf)
          (kill-buffer b))))
    (test-it "rejects a string (e.g. a buffer name)"
      (expect (tt-buf "*l-type-coverage*") :to-equal :other)))

  (describe ":char-table"
    (before-all
      (ldef tt-ct (x :char-table) -> :ct)
      (ldef tt-ct _ -> :other))
    (test-it "matches a char-table"
      (expect (tt-ct (make-char-table 'general-category)) :to-equal :ct))
    (test-it "rejects a regular vector (also an :array)"
      (expect (tt-ct [1 2 3]) :to-equal :other)))

  (describe ":cons"
    (before-all
      (ldef tt-cons (x :cons) -> :cons)
      (ldef tt-cons _ -> :other))
    (test-it "matches a bare cons pair"
      (expect (tt-cons '(1 . 2)) :to-equal :cons))
    (test-it "matches a non-empty list (lists are cons cells)"
      (expect (tt-cons '(1 2 3)) :to-equal :cons))
    (test-it "rejects nil (empty list, not a cons)"
      (expect (tt-cons nil) :to-equal :other)))

  (describe ":float"
    (before-all
      (ldef tt-fl (x :float) -> :fl)
      (ldef tt-fl _ -> :other))
    (test-it "matches a float"
      (expect (tt-fl 3.14) :to-equal :fl))
    (test-it "rejects an integer (also a :number, but not float)"
      (expect (tt-fl 5) :to-equal :other)))

  (describe ":function"
    (before-all
      (ldef tt-fn (x :function) -> :fn)
      (ldef tt-fn _ -> :other))
    (test-it "matches a lambda"
      (expect (tt-fn (lambda (x) x)) :to-equal :fn))
    (test-it "matches a symbol bound to a function"
      (expect (tt-fn 'car) :to-equal :fn))
    (test-it "rejects a symbol with no function binding"
      (expect (tt-fn 'no-such-function-bound) :to-equal :other)))

  (describe ":hash-table"
    (before-all
      (ldef tt-hash (x :hash-table) -> :ht)
      (ldef tt-hash _ -> :other))
    (test-it "matches a hash-table"
      (expect (tt-hash (make-hash-table)) :to-equal :ht))
    (test-it "rejects an alist (the key/value alternative)"
      (expect (tt-hash '((a . 1) (b . 2))) :to-equal :other)))

  (describe ":natural"
    (before-all
      (ldef tt-nat (x :natural) -> :nat)
      (ldef tt-nat _ -> :other))
    (test-it "matches a positive integer"
      (expect (tt-nat 5) :to-equal :nat))
    (test-it "matches a large positive integer"
      (expect (tt-nat 1000000) :to-equal :nat))
    (test-it "rejects zero (boundary case; current predicate is > 0)"
      (expect (tt-nat 0) :to-equal :other))
    (test-it "rejects a negative integer"
      (expect (tt-nat -1) :to-equal :other))
    (test-it "rejects a positive float (must be integer)"
      (expect (tt-nat 2.4) :to-equal :other)))

  (describe ":list"
    (before-all
      (ldef tt-list (x :list) -> :list)
      (ldef tt-list _ -> :other))
    (test-it "matches a non-empty list"
      (expect (tt-list '(1 2 3)) :to-equal :list))
    (test-it "matches nil (empty list)"
      (expect (tt-list nil) :to-equal :list))
    (test-it "rejects a vector (also a :sequence)"
      (expect (tt-list [1 2 3]) :to-equal :other))
    (test-it "rejects a string (also a :sequence)"
      (expect (tt-list "abc") :to-equal :other)))

  (describe ":null"
    (before-all
      (ldef tt-null (x :null) -> :null)
      (ldef tt-null _ -> :other))
    (test-it "matches nil"
      (expect (tt-null nil) :to-equal :null))
    (test-it "matches empty list ()"
      (expect (tt-null '()) :to-equal :null))
    (test-it "rejects 0 (falsy-looking but not nil)"
      (expect (tt-null 0) :to-equal :other))
    (test-it "rejects a non-empty list"
      (expect (tt-null '(1)) :to-equal :other)))

  (describe ":object (EIEIO)"
    (before-all
      (ldef tt-obj (x :object) -> :obj)
      (ldef tt-obj _ -> :other))
    (test-it "matches an EIEIO class instance"
      (expect (tt-obj (l-test-coverage-eieio)) :to-equal :obj))
    (test-it "rejects a cl-defstruct instance (different OO system)"
      (expect (tt-obj (make-l-test-coverage-point :x 1 :y 2)) :to-equal :other)))

  (describe ":plist"
    (before-all
      (ldef tt-plist (x :plist) -> :plist)
      (ldef tt-plist _ -> :other))
    (test-it "matches a property list with keyword keys"
      (expect (tt-plist '(:a 1 :b 2)) :to-equal :plist))
    (test-it "rejects an odd-length list (plistp requires even count)"
      ;; Note: an alist with an even number of pairs is also a plist by
      ;; `plistp's lenient definition (any even-length list).  Pick a
      ;; clearly-not-plist boundary instead.
      (expect (tt-plist '(:a 1 :b)) :to-equal :other))
    (test-it "rejects a vector (not a list at all)"
      (expect (tt-plist [:a 1 :b 2]) :to-equal :other)))

  (describe ":record"
    (before-all
      (ldef tt-rec (x :record) -> :rec)
      (ldef tt-rec _ -> :other))
    (test-it "matches a cl-defstruct instance (structs are records)"
      (expect (tt-rec (make-l-test-coverage-point :x 1 :y 2)) :to-equal :rec))
    (test-it "rejects a regular vector"
      (expect (tt-rec [1 2 3]) :to-equal :other)))

  (describe ":string"
    (before-all
      (ldef tt-str (x :string) -> :str)
      (ldef tt-str _ -> :other))
    (test-it "matches a string"
      (expect (tt-str "hello") :to-equal :str))
    (test-it "matches the empty string"
      (expect (tt-str "") :to-equal :str))
    (test-it "rejects a symbol (looks similar in print)"
      (expect (tt-str 'hello) :to-equal :other))
    (test-it "rejects a list (also a :sequence)"
      (expect (tt-str '("h" "i")) :to-equal :other)))

  (describe ":struct"
    (before-all
      (ldef tt-st (x :struct) -> :st)
      (ldef tt-st _ -> :other))
    (test-it "matches a cl-defstruct instance"
      (expect (tt-st (make-l-test-coverage-point :x 1 :y 2)) :to-equal :st))
    (test-it "rejects an EIEIO object (different OO system)"
      (expect (tt-st (l-test-coverage-eieio)) :to-equal :other)))

  (describe ":symbol"
    (before-all
      (ldef tt-sym (x :symbol) -> :sym)
      (ldef tt-sym _ -> :other))
    (test-it "matches a symbol"
      (expect (tt-sym 'foo) :to-equal :sym))
    (test-it "matches a keyword (keywords are symbols)"
      (expect (tt-sym :bar) :to-equal :sym))
    (test-it "rejects a string (textually similar)"
      (expect (tt-sym "foo") :to-equal :other)))

  (describe ":vector"
    (before-all
      (ldef tt-vec (x :vector) -> :vec)
      (ldef tt-vec _ -> :other))
    (test-it "matches a vector"
      (expect (tt-vec [1 2 3]) :to-equal :vec))
    (test-it "rejects a list (also a :sequence)"
      (expect (tt-vec '(1 2 3)) :to-equal :other))
    (test-it "rejects a string (also an :array)"
      (expect (tt-vec "abc") :to-equal :other)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Category types
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (describe ":array"
    (before-all
      (ldef tt-arr (x :array) -> :arr)
      (ldef tt-arr _ -> :other))
    (test-it "matches a vector"
      (expect (tt-arr [1 2 3]) :to-equal :arr))
    (test-it "matches a string"
      (expect (tt-arr "abc") :to-equal :arr))
    (test-it "matches a bool-vector"
      (expect (tt-arr (make-bool-vector 3 t)) :to-equal :arr))
    (test-it "rejects a list (sequence but not array)"
      (expect (tt-arr '(1 2 3)) :to-equal :other)))

  (describe ":callable"
    (before-all
      (ldef tt-call (x :callable) -> :call)
      (ldef tt-call _ -> :other))
    (test-it "matches a lambda"
      (expect (tt-call (lambda (x) x)) :to-equal :call))
    (test-it "matches a builtin (subr)"
      (expect (tt-call (symbol-function 'car)) :to-equal :call))
    (test-it "rejects a symbol with no function binding"
      (expect (tt-call 'no-such-function-bound) :to-equal :other)))

  (describe ":instance"
    (before-all
      (ldef tt-inst (x :instance) -> :inst)
      (ldef tt-inst _ -> :other))
    (test-it "matches an EIEIO object"
      (expect (tt-inst (l-test-coverage-eieio)) :to-equal :inst))
    (test-it "matches a cl-defstruct instance"
      (expect (tt-inst (make-l-test-coverage-point :x 1 :y 2)) :to-equal :inst))
    (test-it "rejects a plain vector"
      (expect (tt-inst [1 2 3]) :to-equal :other)))

  (describe ":integer"
    (before-all
      (ldef tt-int (x :integer) -> :int)
      (ldef tt-int _ -> :other))
    (test-it "matches a positive integer"
      (expect (tt-int 5) :to-equal :int))
    (test-it "matches zero"
      (expect (tt-int 0) :to-equal :int))
    (test-it "matches a negative integer"
      (expect (tt-int -42) :to-equal :int))
    (test-it "rejects a float (also a :number)"
      (expect (tt-int 2.4) :to-equal :other)))

  (describe ":number"
    (before-all
      (ldef tt-num (x :number) -> :num)
      (ldef tt-num _ -> :other))
    (test-it "matches an integer"
      (expect (tt-num 5) :to-equal :num))
    (test-it "matches a float"
      (expect (tt-num 3.14) :to-equal :num))
    (test-it "rejects a numeric string"
      (expect (tt-num "5") :to-equal :other)))

  (describe ":sequence"
    (before-all
      (ldef tt-seq (x :sequence) -> :seq)
      (ldef tt-seq _ -> :other))
    (test-it "matches a list"
      (expect (tt-seq '(1 2 3)) :to-equal :seq))
    (test-it "matches a vector"
      (expect (tt-seq [1 2 3]) :to-equal :seq))
    (test-it "matches a string"
      (expect (tt-seq "abc") :to-equal :seq))
    (test-it "rejects a hash-table (collection but not a sequence)"
      (expect (tt-seq (make-hash-table)) :to-equal :other))
    (test-it "rejects a number"
      (expect (tt-seq 5) :to-equal :other)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Aliases — smoke test each against its canonical's value
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (describe "aliases match what their canonical matches"
    (before-all
      (ldef tt-str-alias  (x :str)     -> :match)
      (ldef tt-str-alias  _            -> :other)
      (ldef tt-int-alias  (x :int)     -> :match)
      (ldef tt-int-alias  _            -> :other)
      (ldef tt-fn-alias   (x :fn)      -> :match)
      (ldef tt-fn-alias   _            -> :other)
      (ldef tt-nil-alias  (x :nil)     -> :match)
      (ldef tt-nil-alias  _            -> :other)
      (ldef tt-buff-alias (x :buff)    -> :match)
      (ldef tt-buff-alias _            -> :other)
      (ldef tt-bv-alias   (x :bvector) -> :match)
      (ldef tt-bv-alias   _            -> :other)
      (ldef tt-ct-alias   (x :ctable)  -> :match)
      (ldef tt-ct-alias   _            -> :other)
      (ldef tt-seq-alias  (x :seq)     -> :match)
      (ldef tt-seq-alias  _            -> :other))

    (test-it ":str matches a string"
      (expect (tt-str-alias "hello") :to-equal :match))
    (test-it ":int matches an integer"
      (expect (tt-int-alias 5) :to-equal :match))
    (test-it ":fn matches a function"
      (expect (tt-fn-alias (lambda () nil)) :to-equal :match))
    (test-it ":nil matches nil"
      (expect (tt-nil-alias nil) :to-equal :match))
    (test-it ":buff matches a buffer"
      (let ((b (get-buffer-create "*l-type-coverage-alias*")))
        (unwind-protect
            (expect (tt-buff-alias b) :to-equal :match)
          (kill-buffer b))))
    (test-it ":bvector matches a bool-vector"
      (expect (tt-bv-alias (make-bool-vector 3 t)) :to-equal :match))
    (test-it ":ctable matches a char-table"
      (expect (tt-ct-alias (make-char-table 'general-category)) :to-equal :match))
    (test-it ":seq matches a sequence"
      (expect (tt-seq-alias '(1 2 3)) :to-equal :match))))

;;; ldef-type-coverage-test.el ends here
