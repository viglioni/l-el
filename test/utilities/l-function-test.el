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
                :to-equal '(new a b))))))
