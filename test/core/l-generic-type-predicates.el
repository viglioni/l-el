(require 'l-test-helpers)

(context "l-generic-type-predicates"
  (describe ":function"
    (test-it "applies correct method"
      (expect (funcall (alist-get :function l-generic-type-predicates) (lambda (x) x)) :to-be t)
      (expect (funcall (alist-get :function l-generic-type-predicates) 'car) :to-be t)
      (expect (funcall (alist-get :function l-generic-type-predicates) "not-a-function") :to-be nil)
      (expect (funcall (alist-get :function l-generic-type-predicates) 'car-n) :to-be nil)
      (expect (funcall (alist-get :function l-generic-type-predicates) '(1 2 3)) :to-be nil)))

  (describe ":number"
    (test-it "applies correct method"
      (expect (funcall (alist-get :number l-generic-type-predicates) 42) :to-be t)
      (expect (funcall (alist-get :number l-generic-type-predicates) 3.14) :to-be t)
      (expect (funcall (alist-get :number l-generic-type-predicates) -10) :to-be t)
      (expect (funcall (alist-get :number l-generic-type-predicates) "42") :to-be nil)
      (expect (funcall (alist-get :number l-generic-type-predicates) 'forty-two) :to-be nil)
      (expect (funcall (alist-get :number l-generic-type-predicates) '(1 2 3)) :to-be nil)))

  (describe ":integer"
    (test-it "applies correct method"
      (expect (funcall (alist-get :integer l-generic-type-predicates) 42) :to-be t)
      (expect (funcall (alist-get :integer l-generic-type-predicates) -10) :to-be t)
      (expect (funcall (alist-get :integer l-generic-type-predicates) 0) :to-be t)
      (expect (funcall (alist-get :integer l-generic-type-predicates) 3.14) :to-be nil)
      (expect (funcall (alist-get :integer l-generic-type-predicates) "42") :to-be nil)
      (expect (funcall (alist-get :integer l-generic-type-predicates) 'forty-two) :to-be nil)))

  (describe ":float"
    (test-it "applies correct method"
      (expect (funcall (alist-get :float l-generic-type-predicates) 3.14) :to-be t)
      (expect (funcall (alist-get :float l-generic-type-predicates) -2.5) :to-be t)
      (expect (funcall (alist-get :float l-generic-type-predicates) 0.0) :to-be t)
      (expect (funcall (alist-get :float l-generic-type-predicates) 42) :to-be nil)
      (expect (funcall (alist-get :float l-generic-type-predicates) "3.14") :to-be nil)
      (expect (funcall (alist-get :float l-generic-type-predicates) 'pi) :to-be nil)))

  (describe ":string"
    (test-it "applies correct method"
      (expect (funcall (alist-get :string l-generic-type-predicates) "hello") :to-be t)
      (expect (funcall (alist-get :string l-generic-type-predicates) "") :to-be t)
      (expect (funcall (alist-get :string l-generic-type-predicates) "42") :to-be t)
      (expect (funcall (alist-get :string l-generic-type-predicates) 42) :to-be nil)
      (expect (funcall (alist-get :string l-generic-type-predicates) 'hello) :to-be nil)
      (expect (funcall (alist-get :string l-generic-type-predicates) '("hello")) :to-be nil)))

  (describe ":symbol"
    (test-it "applies correct method"
      (expect (funcall (alist-get :symbol l-generic-type-predicates) 'hello) :to-be t)
      (expect (funcall (alist-get :symbol l-generic-type-predicates) 'nil) :to-be t)
      (expect (funcall (alist-get :symbol l-generic-type-predicates) t) :to-be t)
      (expect (funcall (alist-get :symbol l-generic-type-predicates) "hello") :to-be nil)
      (expect (funcall (alist-get :symbol l-generic-type-predicates) 42) :to-be nil)
      (expect (funcall (alist-get :symbol l-generic-type-predicates) '(hello)) :to-be nil)))

  (describe ":list"
    (test-it "applies correct method"
      (expect (funcall (alist-get :list l-generic-type-predicates) '(1 2 3)) :to-be t)
      (expect (funcall (alist-get :list l-generic-type-predicates) '()) :to-be t)
      (expect (funcall (alist-get :list l-generic-type-predicates) nil) :to-be t)
      (expect (funcall (alist-get :list l-generic-type-predicates) [1 2 3]) :to-be nil)
      (expect (funcall (alist-get :list l-generic-type-predicates) "hello") :to-be nil)
      (expect (funcall (alist-get :list l-generic-type-predicates) 42) :to-be nil)))

  (describe ":cons"
    (test-it "applies correct method"
      (expect (funcall (alist-get :cons l-generic-type-predicates) '(1 . 2)) :to-be t)
      (expect (funcall (alist-get :cons l-generic-type-predicates) '(1 2 3)) :to-be t)
      (expect (funcall (alist-get :cons l-generic-type-predicates) '(a . b)) :to-be t)
      (expect (funcall (alist-get :cons l-generic-type-predicates) nil) :to-be nil)
      (expect (funcall (alist-get :cons l-generic-type-predicates) [1 2 3]) :to-be nil)
      (expect (funcall (alist-get :cons l-generic-type-predicates) "hello") :to-be nil)))

  (describe ":vector"
    (test-it "applies correct method"
      (expect (funcall (alist-get :vector l-generic-type-predicates) [1 2 3]) :to-be t)
      (expect (funcall (alist-get :vector l-generic-type-predicates) []) :to-be t)
      (expect (funcall (alist-get :vector l-generic-type-predicates) ["hello" "world"]) :to-be t)
      (expect (funcall (alist-get :vector l-generic-type-predicates) '(1 2 3)) :to-be nil)
      (expect (funcall (alist-get :vector l-generic-type-predicates) "hello") :to-be nil)
      (expect (funcall (alist-get :vector l-generic-type-predicates) 42) :to-be nil)))

  (describe ":hash-table"
    (test-it "applies correct method"
      (let ((ht (make-hash-table)))
        (expect (funcall (alist-get :hash-table l-generic-type-predicates) ht) :to-be t)
        (expect (funcall (alist-get :hash-table l-generic-type-predicates) '((a . 1) (b . 2))) :to-be nil)
        (expect (funcall (alist-get :hash-table l-generic-type-predicates) [1 2 3]) :to-be nil)
        (expect (funcall (alist-get :hash-table l-generic-type-predicates) "hello") :to-be nil))))

  (describe ":buffer"
    (test-it "applies correct method"
      (with-temp-buffer
        (expect (funcall (alist-get :buffer l-generic-type-predicates) (current-buffer)) :to-be t)
        (expect (funcall (alist-get :buffer l-generic-type-predicates) "buffer-name") :to-be nil)
        (expect (funcall (alist-get :buffer l-generic-type-predicates) 42) :to-be nil)
        (expect (funcall (alist-get :buffer l-generic-type-predicates) '(buffer)) :to-be nil))))

  (describe ":callable"
    (test-it "applies correct method"
      (expect (funcall (alist-get :callable l-generic-type-predicates) (lambda (x) x)) :to-be t)
      (expect (funcall (alist-get :callable l-generic-type-predicates) 'car) :to-be t)
      (expect (funcall (alist-get :callable l-generic-type-predicates) '+) :to-be t)
      (expect (funcall (alist-get :callable l-generic-type-predicates) "not-callable") :to-be nil)
      (expect (funcall (alist-get :callable l-generic-type-predicates) 42) :to-be nil)
      (expect (funcall (alist-get :callable l-generic-type-predicates) '(1 2 3)) :to-be nil)))

  (describe ":sequence"
    (test-it "applies correct method"
      (expect (funcall (alist-get :sequence l-generic-type-predicates) '(1 2 3)) :to-be t)
      (expect (funcall (alist-get :sequence l-generic-type-predicates) [1 2 3]) :to-be t)
      (expect (funcall (alist-get :sequence l-generic-type-predicates) "hello") :to-be t)
      (expect (funcall (alist-get :sequence l-generic-type-predicates) nil) :to-be t)
      (expect (funcall (alist-get :sequence l-generic-type-predicates) 'symbol) :to-be nil)
      (expect (funcall (alist-get :sequence l-generic-type-predicates) 42) :to-be nil)))

  (describe ":atom"
    (test-it "applies correct method"
      (expect (funcall (alist-get :atom l-generic-type-predicates) 'symbol) :to-be t)
      (expect (funcall (alist-get :atom l-generic-type-predicates) 42) :to-be t)
      (expect (funcall (alist-get :atom l-generic-type-predicates) "hello") :to-be t)
      (expect (funcall (alist-get :atom l-generic-type-predicates) nil) :to-be t)
      (expect (funcall (alist-get :atom l-generic-type-predicates) '(1 2 3)) :to-be nil)
      (expect (funcall (alist-get :atom l-generic-type-predicates) [1 2 3]) :to-be nil)))

  (describe ":null"
    (test-it "applies correct method"
      (expect (funcall (alist-get :null l-generic-type-predicates) nil) :to-be t)
      (expect (funcall (alist-get :null l-generic-type-predicates) t) :to-be nil)
      (expect (funcall (alist-get :null l-generic-type-predicates) 0) :to-be nil)
      (expect (funcall (alist-get :null l-generic-type-predicates) "") :to-be nil)
      (expect (funcall (alist-get :null l-generic-type-predicates) '()) :to-be nil)
      (expect (funcall (alist-get :null l-generic-type-predicates) 'nil) :to-be nil))))

