;;; -*- lexical-binding: t; -*-

(require 'l-test-helpers)

(context "l-function.el"
  (describe "lcomp with single function"
    (test-it "returns the function unchanged"
             (expect (funcall (lcomp (lambda (x) (+ x 1))) 5) :to-equal 6))

    (test-it "handles string functions"
      (let ((upcase-fn (lambda (s) (upcase s))))
        (expect (funcall (lcomp upcase-fn) "hello") :to-equal "HELLO"))))

  (describe "lcomp with two functions"
    (test-it "composes numeric functions right to left"
      (let ((double (lambda (x) (* x 2)))
            (add-ten (lambda (x) (+ x 10))))
        (expect (funcall (lcomp add-ten double) 5) :to-equal 20))) ; (+ (* 5 2) 10)

    (test-it "composes string functions right to left"
      (let ((add-exclamation (lambda (s) (concat s "!")))
            (upcase-fn (lambda (s) (upcase s))))
        (expect (funcall (lcomp add-exclamation upcase-fn) "hello") :to-equal "HELLO!")))

    (test-it "handles functions with multiple arguments in first application"
      (let ((multiply (lambda (x y) (* x y)))
            (add-one (lambda (x) (+ x 1))))
        (expect (funcall (lcomp add-one multiply) 3 4) :to-equal 13)))) ; (+ (* 3 4) 1)

  (describe "lcomp with multiple functions"
    (test-it "composes three numeric functions correctly"
      (let ((add-one (lambda (x) (+ x 1)))
            (double (lambda (x) (* x 2)))
            (square (lambda (x) (* x x))))
        (expect (funcall (lcomp add-one double square) 3) :to-equal 19))) ; (+ (* (* 3 3) 2) 1)

    (test-it "composes four string transformation functions"
      (let ((add-prefix (lambda (s) (concat "Mr. " s)))
            (add-suffix (lambda (s) (concat s " Jr.")))
            (upcase-fn (lambda (s) (upcase s)))
            (trim-spaces (lambda (s) (string-trim s))))
        (expect (funcall (lcomp add-prefix add-suffix upcase-fn trim-spaces) " john ") 
                :to-equal "Mr. JOHN Jr."))))

  (describe "lcomp with type transformations"
    (test-it "handles number to string to list transformations"
      (let ((to-list (lambda (s) (list s)))
            (to-string (lambda (n) (number-to-string n)))
            (multiply-by-ten (lambda (x) (* x 10))))
        (expect (funcall (lcomp to-list to-string multiply-by-ten) 5) :to-equal '("50"))))

    (test-it "handles list to length to boolean transformation"
      (let ((not-empty-p (lambda (n) (> n 0)))
            (get-length (lambda (lst) (length lst)))
            (add-item (lambda (lst) (cons 'new-item lst))))
        (expect (funcall (lcomp not-empty-p get-length add-item) '(a b)) :to-be t)))

    (test-it "handles complex data structure transformations"
      (let ((get-keys (lambda (alist) (mapcar 'car alist)))
            (add-pair (lambda (alist) (cons '(new . value) alist)))
            (reverse-list (lambda (lst) (reverse lst))))
        (expect (funcall (lcomp get-keys add-pair reverse-list) '((b . 2) (a . 1)))
                :to-equal '(new a b)))))

  (describe "lcomp with __ placeholder syntax"
    (test-it "composes lambda with __ expression"
      (expect (with-l ((lcomp (l x -> (1+ x)) (exp __)) 10))
              :to-equal (1+ (exp 10))))

    (test-it "composes multiple functions with __ in the middle"
      (expect (with-l ((lcomp (l x -> (* x 2)) (1+ __) (exp __)) 5))
              :to-equal (* 2 (1+ (exp 5)))))

    (test-it "uses __ with built-in functions"
      (expect (with-l ((lcomp (l x -> (concat "Result: " x)) (number-to-string __) (* __ 10)) 5))
              :to-equal "Result: 50"))

    (test-it "combines __ with regular lambdas"
      (let ((square (lambda (x) (* x x))))
        (expect (with-l ((lcomp (l x -> (+ x 1)) square (abs __)) -3))
                :to-equal (1+ (* 3 3)))))

    (test-it "uses __ with list operations"
      (expect (with-l ((lcomp
                        (l x -> (length x))
                        (reverse __)
                        #'cdr) '(1 2 3 4)))
              :to-equal (length (reverse (cdr '(1 2 3 4))))))

    (test-it "uses __ multiple times in same expression"
      (expect (with-l ((lcomp (l x -> (* x 2)) (+ __ __)) 5))
              :to-equal 20)))

  (describe "lpipe basic usage"
    (test-it "pipes value through single function"
      (expect (lpipe 5 (lambda (x) (+ x 1))) :to-equal 6))

    (test-it "pipes value through two functions left to right"
      (let ((double (lambda (x) (* x 2)))
            (add-ten (lambda (x) (+ x 10))))
        (expect (lpipe 5 double add-ten) :to-equal 20))) ; (* 5 2) -> 10, (+ 10 10) -> 20

    (test-it "pipes string through transformations"
      (let ((add-exclamation (lambda (s) (concat s "!")))
            (upcase-fn (lambda (s) (upcase s))))
        (expect (lpipe "hello" upcase-fn add-exclamation) :to-equal "HELLO!"))))

  (describe "lpipe with multiple functions"
    (test-it "pipes through three numeric functions"
      (let ((square (lambda (x) (* x x)))
            (double (lambda (x) (* x 2)))
            (add-one (lambda (x) (+ x 1))))
        (expect (lpipe 3 square double add-one) :to-equal 19))) ; 3 -> 9 -> 18 -> 19

    (test-it "pipes through string transformations"
      (let ((trim-spaces (lambda (s) (string-trim s)))
            (upcase-fn (lambda (s) (upcase s)))
            (add-suffix (lambda (s) (concat s " Jr.")))
            (add-prefix (lambda (s) (concat "Mr. " s))))
        (expect (lpipe " john " trim-spaces upcase-fn add-suffix add-prefix)
                :to-equal "Mr. JOHN Jr."))))

  (describe "lpipe with type transformations"
    (test-it "pipes through number to string to list"
      (let ((multiply-by-ten (lambda (x) (* x 10)))
            (to-string (lambda (n) (number-to-string n)))
            (to-list (lambda (s) (list s))))
        (expect (lpipe 5 multiply-by-ten to-string to-list) :to-equal '("50"))))

    (test-it "pipes through list operations"
      (let ((add-item (lambda (lst) (cons 'new-item lst)))
            (get-length (lambda (lst) (length lst)))
            (not-empty-p (lambda (n) (> n 0))))
        (expect (lpipe '(a b) add-item get-length not-empty-p) :to-be t)))

    (test-it "pipes through complex data transformations"
      (let ((reverse-list (lambda (lst) (reverse lst)))
            (add-pair (lambda (alist) (cons '(new . value) alist)))
            (get-keys (lambda (alist) (mapcar 'car alist))))
        (expect (lpipe '((a . 1) (b . 2)) reverse-list add-pair get-keys)
                :to-equal '(new b a)))))

  (describe "lpipe with __ placeholder syntax"
    (test-it "pipes with __ expression"
      (expect (lpipe 10 (exp __) (l x -> (1+ x)))
              :to-equal (1+ (exp 10))))

    (test-it "pipes with multiple __ functions"
      (expect (lpipe 5 (exp __) (1+ __) (l x -> (* x 2)))
              :to-equal (* 2 (1+ (exp 5)))))

    (test-it "pipes with built-in functions using __"
      (expect (lpipe 5 (* __ 10) (number-to-string __) (l x -> (concat "Result: " x)))
              :to-equal "Result: 50"))

    (test-it "pipes combining __ with regular lambdas"
      (let ((square (lambda (x) (* x x))))
        (expect (lpipe -3 (abs __) square (l x -> (+ x 1)))
                :to-equal (1+ (* 3 3)))))

    (test-it "pipes with list operations using __"
      (expect (lpipe '(1 2 3 4) #'cdr (reverse __) (l x -> (length x)))
              :to-equal (length (reverse (cdr '(1 2 3 4))))))

    (test-it "pipes with __ used multiple times in expression"
      (expect (lpipe 5 (+ __ __) (l x -> (* x 2)))
              :to-equal 20)))

  (describe "lpipe vs lcomp comparison"
    (test-it "produces same result as lcomp but with reversed order"
      (let ((f1 (lambda (x) (* x 2)))
            (f2 (lambda (x) (+ x 10)))
            (f3 (lambda (x) (- x 5))))
        (expect (lpipe 5 f1 f2 f3) :to-equal (funcall (lcomp f3 f2 f1) 5))))

    (test-it "lpipe evaluates left to right, lcomp right to left"
      (expect (lpipe 3 (l x -> (* x 2)) (l x -> (+ x 10)))
              :to-equal (funcall (lcomp (l x -> (+ x 10)) (l x -> (* x 2))) 3))))

  (describe "lpipe edge cases"
    (test-it "works with no functions (returns value unchanged)"
      (expect (lpipe 42) :to-equal 42))

    (test-it "works with identity function"
      (expect (lpipe 10 (l x -> x)) :to-equal 10))

    (test-it "works with nil as initial value"
      (expect (lpipe nil (l x -> (or x 'default))) :to-equal 'default))

    (test-it "works with complex nested data"
      (let ((extract-name (lambda (person) (cdr (assq 'name person))))
            (upcase-name (lambda (name) (upcase name))))
        (expect (lpipe '((name . "john") (age . 30)) extract-name upcase-name)
                :to-equal "JOHN"))))

  (describe "lcomp error cases"
    (test-it "raises error when passing non-function to composed result"
      (expect (funcall (lcomp (l x -> (+ x 1)) 'not-a-function) 5)
              :to-throw 'l-pattern-match-error))

    (test-it "raises error when passing symbol that is not a function"
      (expect (funcall (lcomp (l x -> (* x 2)) 'undefined-symbol) 10)
              :to-throw 'l-pattern-match-error))

    (test-it "raises error when passing number as function"
      (expect (funcall (lcomp 42 (l x -> (+ x 1))) 5)
              :to-throw 'l-pattern-match-error))

    (test-it "raises error when passing string as function"
      (expect (funcall (lcomp "not-a-function" (l x -> (+ x 1))) 5)
              :to-throw 'l-pattern-match-error))

    (test-it "raises error when passing list as function"
      (expect (funcall (lcomp '(1 2 3) (l x -> (+ x 1))) 5)
              :to-throw 'l-pattern-match-error)))

  (describe "lpipe error cases"
    (test-it "raises error when passing non-function"
      (expect (lpipe 5 'not-a-function)
              :to-throw 'l-pattern-match-error))

    (test-it "raises error when passing symbol that is not a function"
      (expect (lpipe 10 'undefined-symbol)
              :to-throw 'l-pattern-match-error))

    (test-it "raises error when passing number as function"
      (expect (lpipe 5 42)
              :to-throw 'l-pattern-match-error))

    (test-it "raises error when passing string as function"
      (expect (lpipe 5 "not-a-function")
              :to-throw 'l-pattern-match-error))

    (test-it "raises error when passing list as function in middle of pipe"
      (expect (lpipe 5 (l x -> (* x 2)) '(1 2 3) (l x -> (+ x 1)))
              :to-throw 'l-pattern-match-error))))
