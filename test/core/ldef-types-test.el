;;; -*- lexical-binding: t; -*-

(require 'l-test-helpers)

(context "ldef type matching"
  (describe "basic type matching"
    (before-all
      (ldef type-add ((x :integer) (y :integer)) (+ x y))
      (ldef type-add (x y) "not integers"))

    (test-it "matches integer types"
      (expect (type-add 5 3) :to-equal 8)
      (expect (type-add 0 0) :to-equal 0)
      (expect (type-add -1 1) :to-equal 0))

    (test-it "falls through to general case for non-integers"
      (expect (type-add "hello" "world") :to-equal "not integers")
      (expect (type-add 5 "hello") :to-equal "not integers")
      (expect (type-add '(1 2) 3) :to-equal "not integers"))

    (test-it "works with currying on type-matched functions"
      (expect (funcall (type-add 5) 3) :to-equal 8)
      (expect (funcall (type-add "hello") "world") :to-equal "not integers")))

  (describe "multiple type specializers"
    (before-all
      (ldef multi-type ((x :string) (y :integer)) (concat x " " (number-to-string y)))
      (ldef multi-type ((x :integer) (y :string)) (concat (number-to-string x) " " y))
      (ldef multi-type (x y) "mixed types"))

    (test-it "matches string-integer combination"
      (expect (multi-type "count" 42) :to-equal "count 42"))

    (test-it "matches integer-string combination"
      (expect (multi-type 42 "items") :to-equal "42 items"))

    (test-it "falls through for other combinations"
      (expect (multi-type 42 42) :to-equal "mixed types")
      (expect (multi-type "hello" "world") :to-equal "mixed types")))

  (describe "various type specializers"
    (before-all
      (ldef type-processor ((x :symbol)) (symbol-name x))
      (ldef type-processor ((x :string)) (upcase x))
      (ldef type-processor ((x :list)) (length x))
      (ldef type-processor ((x nil)) (length x))
      (ldef type-processor (x) "unknown type"))

    (test-it "matches symbol type"
      (expect (type-processor 'hello) :to-equal "hello")
      (expect (type-processor 'world) :to-equal "world"))

    (test-it "matches string type"
      (expect (type-processor "hello") :to-equal "HELLO")
      (expect (type-processor "world") :to-equal "WORLD"))

    (test-it "matches list type"
      (expect (type-processor '(1 2 3)) :to-equal 3)
      (expect (type-processor '()) :to-equal 0))

    (test-it "falls through for other types"
      (expect (type-processor 42) :to-equal "unknown type")
      (expect (type-processor t) :to-equal "t")))

  (describe "array type matcher"
    (before-all
      (ldef array-handler ((x :array)) (cons 'array (length x)))
      (ldef array-handler (x) 'not-array))

    (test-it "matches vectors"
      (expect (array-handler [1 2 3]) :to-equal '(array . 3))
      (expect (array-handler []) :to-equal '(array . 0)))

    (test-it "matches strings"
      (expect (array-handler "hello") :to-equal '(array . 5))
      (expect (array-handler "") :to-equal '(array . 0)))

    (test-it "does not match lists"
      (expect (array-handler '(1 2 3)) :to-equal 'not-array))

    (test-it "does not match non-arrays"
      (expect (array-handler 42) :to-equal 'not-array)
      (expect (array-handler 'symbol) :to-equal 'not-array)))

  (describe "sequence type matcher"
    (before-all
      (ldef seq-handler ((x :sequence)) (cons 'sequence (length x)))
      (ldef seq-handler (x) 'not-sequence))

    (test-it "matches lists"
      (expect (seq-handler '(1 2 3)) :to-equal '(sequence . 3))
      (expect (seq-handler '()) :to-equal '(sequence . 0)))

    (test-it "matches vectors"
      (expect (seq-handler [1 2 3]) :to-equal '(sequence . 3))
      (expect (seq-handler []) :to-equal '(sequence . 0)))

    (test-it "matches strings"
      (expect (seq-handler "hello") :to-equal '(sequence . 5))
      (expect (seq-handler "") :to-equal '(sequence . 0)))

    (test-it "does not match non-sequences"
      (expect (seq-handler 42) :to-equal 'not-sequence)
      (expect (seq-handler 'symbol) :to-equal 'not-sequence)))

  (describe "mixed type and value matching"
    (before-all
      (ldef mixed-matcher ((x :integer) (y 0)) "integer and zero")
      (ldef mixed-matcher ((x :string) (y "test")) "string and test")
      (ldef mixed-matcher ((x :integer) y) (+ x y))
      (ldef mixed-matcher (x y) "fallback"))

    (test-it "matches type-value combination"
      (expect (mixed-matcher 42 0) :to-equal "integer and zero")
      (expect (mixed-matcher "hello" "test") :to-equal "string and test"))

    (test-it "matches type with general value"
      (expect (mixed-matcher 10 5) :to-equal 15)
      (expect (mixed-matcher 7 3) :to-equal 10))

    (test-it "falls through to general case"
      (expect (mixed-matcher "hello" "world") :to-equal "fallback")
      (expect (mixed-matcher '(1 2) 3) :to-equal "fallback")))

  (describe "alist type matcher"
    (before-all
      (ldef alist-handler ((x :alist)) (cons 'alist (length x)))
      (ldef alist-handler (x) 'not-alist))

    (test-it "matches alists"
      (expect (alist-handler '((a . 1) (b . 2))) :to-equal '(alist . 2))
      (expect (alist-handler '((name . "Alice") (age . 30))) :to-equal '(alist . 2)))

    (test-it "matches empty alist"
      (expect (alist-handler '()) :to-equal '(alist . 0)))

    (test-it "does not match lists with non-cons elements"
      (expect (alist-handler '((a . 1) b)) :to-equal 'not-alist)
      (expect (alist-handler '(1 2 3)) :to-equal 'not-alist))

    (test-it "does not match non-lists"
      (expect (alist-handler "string") :to-equal 'not-alist)
      (expect (alist-handler 42) :to-equal 'not-alist)))

  (describe "plist type matcher"
    (before-all
      (ldef plist-handler ((x :plist)) (cons 'plist (/ (length x) 2)))
      (ldef plist-handler (x) 'not-plist))

    (test-it "matches plists"
      (expect (plist-handler '(:name "Alice" :age 30)) :to-equal '(plist . 2))
      (expect (plist-handler '(:a 1 :b 2 :c 3)) :to-equal '(plist . 3)))

    (test-it "matches empty plist"
      (expect (plist-handler '()) :to-equal '(plist . 0)))

    (test-it "does not match lists with odd length"
      (expect (plist-handler '(:a 1 :b)) :to-equal 'not-plist))

    (test-it "does not match non-lists"
      (expect (plist-handler "string") :to-equal 'not-plist)
      (expect (plist-handler 42) :to-equal 'not-plist)))

  (describe "struct type matcher"
    (before-all
      (cl-defstruct test-struct name age)
      (ldef struct-handler ((x :struct)) (cons 'struct (type-of x)))
      (ldef struct-handler (x) 'not-struct))

    (test-it "matches cl-defstruct instances"
      (let ((obj (make-test-struct :name "Alice" :age 30)))
        (expect (struct-handler obj) :to-equal '(struct . test-struct))))

    (test-it "does not match non-structs"
      (expect (struct-handler '(1 2 3)) :to-equal 'not-struct)
      (expect (struct-handler "string") :to-equal 'not-struct)
      (expect (struct-handler 42) :to-equal 'not-struct)))

  (describe "object type matcher"
    (before-all
      (defclass test-class ()
        ((name :initarg :name)
         (age :initarg :age)))
      (ldef object-handler ((x :object)) (cons 'object (eieio-object-class x)))
      (ldef object-handler (x) 'not-object))

    (test-it "matches EIEIO class instances"
      (let ((obj (make-instance 'test-class :name "Alice" :age 30)))
        (expect (object-handler obj) :to-equal '(object . test-class))))

    (test-it "does not match non-objects"
      (expect (object-handler '(1 2 3)) :to-equal 'not-object)
      (expect (object-handler "string") :to-equal 'not-object)
      (expect (object-handler 42) :to-equal 'not-object)))

  (describe "record type matcher"
    (before-all
      (cl-defstruct test-record-struct field)
      (defclass test-record-class ()
        ((field :initarg :field)))
      (ldef record-handler ((x :record)) (cons 'record (type-of x)))
      (ldef record-handler (x) 'not-record))

    (test-it "matches cl-defstruct instances"
      (let ((obj (make-test-record-struct :field 42)))
        (expect (car (record-handler obj)) :to-equal 'record)))

    (test-it "matches EIEIO class instances"
      (let ((obj (make-instance 'test-record-class :field 42)))
        (expect (car (record-handler obj)) :to-equal 'record)))

    (test-it "does not match non-records"
      (expect (record-handler '(1 2 3)) :to-equal 'not-record)
      (expect (record-handler "string") :to-equal 'not-record)
      (expect (record-handler 42) :to-equal 'not-record)))

  (describe "instance type matcher"
    (before-all
      (cl-defstruct test-instance-struct data)
      (defclass test-instance-class ()
        ((data :initarg :data)))
      (ldef instance-handler ((x :instance)) (cons 'instance (type-of x)))
      (ldef instance-handler (x) 'not-instance))

    (test-it "matches cl-defstruct instances"
      (let ((obj (make-test-instance-struct :data 42)))
        (expect (car (instance-handler obj)) :to-equal 'instance)))

    (test-it "matches EIEIO class instances"
      (let ((obj (make-instance 'test-instance-class :data 42)))
        (expect (car (instance-handler obj)) :to-equal 'instance)))

    (test-it "does not match non-instances"
      (expect (instance-handler '(1 2 3)) :to-equal 'not-instance)
      (expect (instance-handler "string") :to-equal 'not-instance)
      (expect (instance-handler 42) :to-equal 'not-instance)))

  (describe "instance_of type matcher (parameterized)"
    (before-all
      (cl-defstruct point x y)
      (cl-defstruct circle center radius)
      (defclass person ()
        ((name :initarg :name)
         (age :initarg :age)))
      (defclass employee ()
        ((id :initarg :id)))

      (ldef shape-processor ((s :instance_of point)) "processing point")
      (ldef shape-processor ((s :instance_of circle)) "processing circle")
      (ldef shape-processor ((s :struct)) "processing generic struct")
      (ldef shape-processor (s) "not a struct"))

    (test-it "matches specific struct type"
      (let ((p (make-point :x 10 :y 20)))
        (expect (shape-processor p) :to-equal "processing point")))

    (test-it "matches different struct type"
      (let ((c (make-circle :center '(0 0) :radius 5)))
        (expect (shape-processor c) :to-equal "processing circle")))

    (test-it "falls through to generic struct for unmatched struct types"
      (cl-defstruct triangle a b c)
      (let ((t1 (make-triangle :a 1 :b 2 :c 3)))
        (expect (shape-processor t1) :to-equal "processing generic struct")))

    (test-it "falls through to catch-all for non-structs"
      (expect (shape-processor "string") :to-equal "not a struct")
      (expect (shape-processor 42) :to-equal "not a struct"))

    (test-it "works with EIEIO classes"
      (ldef person-processor ((p :instance_of person)) "processing person")
      (ldef person-processor ((e :instance_of employee)) "processing employee")
      (ldef person-processor (x) "not a person or employee")

      (let ((p (make-instance 'person :name "Alice" :age 30))
            (e (make-instance 'employee :id 123)))
        (expect (person-processor p) :to-equal "processing person")
        (expect (person-processor e) :to-equal "processing employee")))

    (test-it "specificity: instance_of > generic type > wildcard"
      (ldef specificity-test ((x :instance_of point)) "specific point")
      (ldef specificity-test ((x :struct)) "any struct")
      (ldef specificity-test (x) "anything")

      (let ((p (make-point :x 1 :y 2))
            (c (make-circle :center '(0 0) :radius 1)))
        (expect (specificity-test p) :to-equal "specific point")
        (expect (specificity-test c) :to-equal "any struct")
        (expect (specificity-test "string") :to-equal "anything"))))

  (describe "list_of type matcher (parameterized)"
    (before-all
      (ldef sum-integers ((nums :list_of :integer)) (apply #'+ nums))
      (ldef sum-integers (nums) "not a list of integers")

      (ldef process-strings ((strs :list_of :string)) (mapconcat #'upcase strs " "))
      (ldef process-strings (strs) "not a list of strings"))

    (test-it "matches list of integers"
      (expect (sum-integers '(1 2 3 4 5)) :to-equal 15)
      (expect (sum-integers '(10 20)) :to-equal 30))

    (test-it "matches empty list"
      (expect (sum-integers '()) :to-equal 0))

    (test-it "does not match list with mixed types"
      (expect (sum-integers '(1 2 "3")) :to-equal "not a list of integers"))

    (test-it "does not match non-list"
      (expect (sum-integers "hello") :to-equal "not a list of integers")
      (expect (sum-integers 42) :to-equal "not a list of integers"))

    (test-it "matches list of strings"
      (expect (process-strings '("hello" "world")) :to-equal "HELLO WORLD")
      (expect (process-strings '("foo")) :to-equal "FOO"))

    (test-it "works with other type keywords"
      (ldef process-symbols ((syms :list_of :symbol)) (length syms))
      (ldef process-symbols (x) -1)

      (expect (process-symbols '(a b c)) :to-equal 3)
      (expect (process-symbols '(foo bar)) :to-equal 2)
      (expect (process-symbols '(a "b")) :to-equal -1))

    (test-it "specificity: list_of > generic list > wildcard"
      (ldef list-processor ((lst :list_of :integer)) "list of integers")
      (ldef list-processor ((lst :list)) "any list")
      (ldef list-processor (x) "anything")

      (expect (list-processor '(1 2 3)) :to-equal "list of integers")
      (expect (list-processor '(1 "2" 3)) :to-equal "any list")
      (expect (list-processor "string") :to-equal "anything"))))
