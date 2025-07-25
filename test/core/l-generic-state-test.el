;;; -*- lexical-binding: t; -*-
;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: 0.3.0
;;
;; l-test.el:
;; Tests for l.el
;;
;; The l-generic-state.el module provides the core data structures and state
;; management for the l-generic system. It implements the registry mechanism
;; that stores and manages generic function methods.
;;
;; Key Components:
;;
;; 1. l-generic-method-registry - A global hash table that stores all registered
;;    methods for generic functions
;; 2. l-generic-method-spec - A struct that encapsulates method specifications
;;    with their metadata
;; 3. Registry management functions - Functions to add and retrieve methods
;;    from the registry
;; 4. Accessor methods - Clean interface for accessing method specification
;;    fields
;;
;; Data Structure:
;;
;; The registry maps function names to lists of method specifications, where
;; each specification contains:
;; - specificity: Numeric score for pattern matching priority
;; - arity: Number of arguments the method accepts
;; - pattern-list: The actual patterns to match against
;; - body: Code to execute when the pattern matches
;;

;;; code:

(require 'l-test-helpers)


(context "l-generic-state.el"
  (describe "l-generic-method-registry"
    (before-each
      ;; Clear registry for clean tests
      (clrhash l-generic-method-registry))
    
    (test-it "starts empty"
      (expect (hash-table-count l-generic-method-registry) :to-equal 0))
    
    (test-it "can store and retrieve items"
      (let ((methods (list "method1" "method2")))
        (l--add-to-registry 'test-func methods)
        (expect (l--get-from-registry 'test-func) :to-equal methods)))
    
    (test-it "returns empty list for non-existent keys"
      (expect (l--get-from-registry 'non-existent) :to-equal '()))
    
    (test-it "can store multiple functions"
      (l--add-to-registry 'func1 '("method1"))
      (l--add-to-registry 'func2 '("method2"))
      (expect (l--get-from-registry 'func1) :to-equal '("method1"))
      (expect (l--get-from-registry 'func2) :to-equal '("method2")))
    
    (test-it "overwrites existing entries"
      (l--add-to-registry 'test-func '("old-method"))
      (l--add-to-registry 'test-func '("new-method"))
      (expect (l--get-from-registry 'test-func) :to-equal '("new-method")))
    
    (test-it "handles symbol keys correctly"
      (l--add-to-registry 'symbol-key '("value"))
      (expect (l--get-from-registry 'symbol-key) :to-equal '("value")))
    
    (test-it "handles string keys correctly"
      (l--add-to-registry "string-key" '("value"))
      (expect (l--get-from-registry "string-key") :to-equal '("value"))))

  (describe "l-generic-method-spec struct"
    (test-it "can be created with all fields"
      (let ((spec (l--method 2 '((+ x y)) '((x :integer) (y :integer)) 200)))
        (expect (l-generic-method-spec-p spec) :to-be t)
        (expect (l--arity spec) :to-equal 2)
        (expect (l--body spec) :to-equal '((+ x y)))
        (expect (l--pattern-list spec) :to-equal '((x :integer) (y :integer)))
        (expect (l--specificity spec) :to-equal 200)))
    
    (test-it "constructor creates valid struct"
      (let ((spec (l--method 1 '((* x x)) '((x :number)) 100)))
        (expect (l-generic-method-spec-p spec) :to-be t)))
    
    (test-it "handles zero arity"
      (let ((spec (l--method 0 '(42) '() 1)))
        (expect (l--arity spec) :to-equal 0)
        (expect (l--body spec) :to-equal '(42))
        (expect (l--pattern-list spec) :to-equal '())
        (expect (l--specificity spec) :to-equal 1)))
    
    (test-it "handles complex patterns"
      (let* ((patterns '((x :integer) (y "hello") (z :string)))
             (body '((concat (number-to-string x) y z)))
             (spec (l--method 3 body patterns 1100)))
        (expect (l--pattern-list spec) :to-equal patterns)
        (expect (l--body spec) :to-equal body)))
    
    (test-it "handles rest patterns"
      (let* ((patterns '((x :integer) (y :integer) (rest :rest)))
             (spec (l--method 3 '((list x y rest)) patterns 102)))
        (expect (l--pattern-list spec) :to-equal patterns)
        (expect (l--arity spec) :to-equal 3)))
    
    (test-it "handles wildcard patterns"
      (let* ((patterns '(_ignore (x :integer) _unused))
             (spec (l--method 3 '((* x 2)) patterns 101)))
        (expect (l--pattern-list spec) :to-equal patterns))))

  (describe "accessor methods"
    (before-all
      (setq test-spec (l--method 2 '((+ x y)) '((x :integer) (y :integer)) 200)))
    
    (test-it "l--arity returns correct arity"
      (expect (l--arity test-spec) :to-equal 2))
    
    (test-it "l--body returns correct body"
      (expect (l--body test-spec) :to-equal '((+ x y))))
    
    (test-it "l--pattern-list returns correct pattern list"
      (expect (l--pattern-list test-spec) :to-equal '((x :integer) (y :integer))))
    
    (test-it "l--specificity returns correct specificity"
      (expect (l--specificity test-spec) :to-equal 200))
    
    (test-it "accessors work with different data types"
      (let ((spec (l--method 1 '((list x)) '((x :list)) 100)))
        (expect (l--arity spec) :to-equal 1)
        (expect (l--body spec) :to-equal '((list x)))
        (expect (l--pattern-list spec) :to-equal '((x :list)))
        (expect (l--specificity spec) :to-equal 100))))

  (describe "integration with registry"
    (before-each
      (clrhash l-generic-method-registry))
    
    (test-it "can store and retrieve method specs"
      (let ((spec1 (l--method 1 '((+ x 1)) '((x :integer)) 100))
            (spec2 (l--method 1 '((symbol-name x)) '((x :symbol)) 100)))
        (l--add-to-registry 'test-func (list spec1 spec2))
        (let ((retrieved (l--get-from-registry 'test-func)))
          (expect (length retrieved) :to-equal 2)
          (expect (l--arity (car retrieved)) :to-equal 1)
          (expect (l--arity (cadr retrieved)) :to-equal 1))))
    
    (test-it "preserves method spec structure"
      (let ((original-spec (l--method 3 '((+ x y z)) '(x y z) 3)))
        (l--add-to-registry 'preserve-test (list original-spec))
        (let ((retrieved-spec (car (l--get-from-registry 'preserve-test))))
          (expect (l--arity retrieved-spec) :to-equal (l--arity original-spec))
          (expect (l--body retrieved-spec) :to-equal (l--body original-spec))
          (expect (l--pattern-list retrieved-spec) :to-equal (l--pattern-list original-spec))
          (expect (l--specificity retrieved-spec) :to-equal (l--specificity original-spec)))))
    
    (test-it "handles empty method lists"
      (l--add-to-registry 'empty-func '())
      (expect (l--get-from-registry 'empty-func) :to-equal '()))
    
    (test-it "handles multiple method specs with different specificities"
      (let ((high-spec (l--method 2 '((+ x y)) '((x :integer) (y :integer)) 200))
            (low-spec (l--method 2 '((list x y)) '(x y) 2)))
        (l--add-to-registry 'multi-spec (list high-spec low-spec))
        (let ((retrieved (l--get-from-registry 'multi-spec)))
          (expect (length retrieved) :to-equal 2)
          (expect (l--specificity (car retrieved)) :to-equal 200)
          (expect (l--specificity (cadr retrieved)) :to-equal 2)))))

  (describe "edge cases and error handling"
    (test-it "handles nil values gracefully"
      (l--add-to-registry 'nil-test nil)
      (expect (l--get-from-registry 'nil-test) :to-equal nil))
    
    (test-it "handles method spec with nil body"
      (let ((spec (l--method 0 nil '() 1)))
        (expect (l--body spec) :to-equal nil)))
    
    (test-it "handles method spec with empty pattern list"
      (let ((spec (l--method 0 '(42) '() 1)))
        (expect (l--pattern-list spec) :to-equal '())))
    
    (test-it "handles very high specificity values"
      (let ((spec (l--method 1 '(x) '(x) 999999)))
        (expect (l--specificity spec) :to-equal 999999)))
    
    (test-it "handles negative specificity values"
      (let ((spec (l--method 1 '(x) '(x) -100)))
        (expect (l--specificity spec) :to-equal -100)))
    
    (test-it "registry persists across multiple operations"
      (l--add-to-registry 'persist-test '("method1"))
      (l--add-to-registry 'persist-test2 '("method2"))
      (expect (l--get-from-registry 'persist-test) :to-equal '("method1"))
      (expect (l--get-from-registry 'persist-test2) :to-equal '("method2"))
      (l--add-to-registry 'persist-test3 '("method3"))
      (expect (l--get-from-registry 'persist-test) :to-equal '("method1"))
      (expect (l--get-from-registry 'persist-test2) :to-equal '("method2"))
      (expect (l--get-from-registry 'persist-test3) :to-equal '("method3")))))
