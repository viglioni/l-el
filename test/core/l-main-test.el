;;; -*- lexical-binding: t; -*-
;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; l-test.el:
;; Tests for l.el
;;

;;; code:

(require 'l-test-helpers)

(context "l.el"
  (describe "lpartial"
    (test-it "creates a partial function correctly"
             (let ((partial-fn (lpartial '+ 5)))
               (expect (funcall partial-fn 3) :to-equal 8)))
    
    (test-it "works with multiple initial arguments"
             (let ((partial-fn (lpartial '* 2 3)))
               (expect (funcall partial-fn 4) :to-equal 24)))

    (test-it "works with no initial arguments"
             (let ((partial-fn (lpartial '+)))
               (expect (funcall partial-fn 2 3) :to-equal 5)))

    (test-it "works with functions that return functions"
             (let ((partial-fn (lpartial 'lpartial '+ 5)))
               (expect (funcall (funcall partial-fn) 3) :to-equal 8)))

    (test-it "works with string functions"
             (let ((partial-fn (lpartial 'concat "Hello, ")))
               (expect (funcall partial-fn "World!") :to-equal "Hello, World!")))

    (test-it "works with list functions"
             (let ((partial-fn (lpartial 'append '(1 2))))
               (expect (funcall partial-fn '(3 4)) :to-equal '(1 2 3 4)))))

  (context "ldef"
    (describe "basic function definition"
      (test-it "defines a function that works with all arguments provided"
               (ldef test-add (x y) (+ x y))
               (expect (test-add 3 4) :to-equal 7))
      
      (test-it "works with single argument functions"
               (ldef test-square (x) (* x x))
               (expect (test-square 5) :to-equal 25))
      
      (test-it "works with multiple argument functions"
               (ldef test-multiply (x y z) (* x y z))
               (expect (test-multiply 2 3 4) :to-equal 24)))

    (describe "edge cases"
      (test-it "works with zero argument functions"
               (ldef test-constant () 42)
               (expect (test-constant) :to-equal 42))

      (test-it "works with functions that return complex data"
               (ldef test-list-maker (x y z) (list x y z))
               (expect (test-list-maker 1 2 3) :to-equal '(1 2 3)))      

      (test-it "works with functions that modify arguments"
               (ldef test-modifier (x y) (cons (1+ x) (1+ y)))
               (expect (test-modifier 1 2) :to-equal '(2 . 3)))

      (test-it "works with functions that call other functions"
               (ldef test-caller (x y) (test-add x y))
               (expect (test-caller 5 7) :to-equal 12)))

    (describe "currying behavior"
      (before-all
        (ldef add3 (x y z) (+ x y z)))
      
      (test-it "works with all arguments at once"
               (expect (add3 1 2 3) :to-equal 6))
      
      (test-it "works with partial application - 1 arg then 2"
               (expect (funcall (add3 1) 2 3) :to-equal 6))
      
      (test-it "works with partial application - 2 args then 1"
               (expect (funcall (add3 1 2) 3) :to-equal 6))
      
      (test-it "works with chained partial applications"
               (expect (funcall (funcall (add3 1) 2) 3) :to-equal 6))
      
      (test-it "works with full currying chain"
               (expect (funcall (funcall (funcall (add3) 1) 2) 3) :to-equal 6)))

    (describe "advanced currying scenarios"
      (before-all
        (ldef multiply4 (w x y z) (* w x y z))
        (ldef concat3 (a b c) (concat a b c)))

      (test-it "works with 4-argument functions"
               (expect (multiply4 2 3 4 5) :to-equal 120))

      (test-it "works with 4-argument partial application"
               (expect (funcall (multiply4 2 3) 4 5) :to-equal 120))

      (test-it "works with string concatenation currying"
               (expect (funcall (concat3 "Hello") " " "World") :to-equal "Hello World"))

      (test-it "works with mixed data types in currying"
               (ldef mixed-fn (num str list) (list num str (length list)))
               (expect (funcall (mixed-fn 42) "test" '(1 2 3)) :to-equal '(42 "test" 3))))

    (describe "pattern matching"
      (before-all
        (ldef fib ((n 0)) 0)
        (ldef fib ((n 1)) 1)
        (ldef fib (n) (+ (fib (- n 1)) (fib (- n 2)))))

      (test-it "matches base cases"
               (expect (fib 0) :to-equal 0)
               (expect (fib 1) :to-equal 1))

      (test-it "uses general case for n>=2"
               (expect (fib 2) :to-equal 1)
               (expect (fib 3) :to-equal 2)
               (expect (fib 4) :to-equal 3)
               (expect (fib 5) :to-equal 5))

      (test-it "works with currying on pattern matched functions"
               (expect (funcall (fib) 3) :to-equal 2))      

      (describe "pattern matching with different types"
        (before-all
          (ldef type-checker ((x nil)) "nil")
          (ldef type-checker ((x t)) "true")
          (ldef type-checker ((x 0)) "zero")
          (ldef type-checker (x) "other"))

        (test-it "matches nil value"
                 (expect (type-checker nil) :to-equal "nil"))

        (test-it "matches true value"
                 (expect (type-checker t) :to-equal "true"))

        (test-it "matches zero value"
                 (expect (type-checker 0) :to-equal "zero"))

        (test-it "uses general case for other values"
                 (expect (type-checker 42) :to-equal "other")
                 (expect (type-checker "hello") :to-equal "other")
                 (expect (type-checker '(1 2 3)) :to-equal "other")))

      (describe "pattern matching with strings"
        (before-all
          (ldef greet ((name "Alice")) "Hello, Alice!")
          (ldef greet ((name "Bob")) "Hey, Bob!")
          (ldef greet (name) (concat "Hi, " name "!")))

        (test-it "matches specific string patterns"
                 (expect (greet "Alice") :to-equal "Hello, Alice!")
                 (expect (greet "Bob") :to-equal "Hey, Bob!"))

        (test-it "uses general case for other strings"
                 (expect (greet "Charlie") :to-equal "Hi, Charlie!")))

      (describe "pattern matching with multiple arguments"
        (before-all
          (ldef calculator ((op '+) x y) (+ x y))
          (ldef calculator ((op '-) x y) (- x y))
          (ldef calculator ((op '*) x y) (* x y))
          (ldef calculator (op x y) (error "Unknown operation: %s" op)))

        (test-it "matches addition operation"
                 (expect (calculator '+ 3 4) :to-equal 7))

        (test-it "matches subtraction operation"
                 (expect (calculator '- 10 3) :to-equal 7))

        (test-it "matches multiplication operation"
                 (expect (calculator '* 5 6) :to-equal 30))

        (test-it "works with currying on pattern matched multi-arg functions"
                 (expect (funcall (calculator '+) 2 3) :to-equal 5)
                 (expect (funcall (calculator '+ 2) 3) :to-equal 5))))

    (describe "type matching"
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
               (expect (funcall (type-add "hello") "world") :to-equal "not integers"))

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
                 (expect (mixed-matcher '(1 2) 3) :to-equal "fallback"))))

    (describe "do not allow more arguments than it is defined"
      (before-all
        (ldef one-or-two-params ((n :number)) n)
        (ldef one-or-two-params ((n :number) (s :string))
              (format "%s. %s" n s)))
      
      (test-it "works with one param"
        (expect (one-or-two-params 10) :to-equal 10))
      
      (test-it "works with two params"
        (expect (one-or-two-params 10 "pelé") :to-equal "10. pelé"))
      
      (test-it "raises for other cases"
        (expect (one-or-two-params "maradona") :to-throw)
        (expect (one-or-two-params 10 10) :to-throw)
        (expect (one-or-two-params "maradona" "rooney") :to-throw)
        (expect (one-or-two-params 10 "garrincha" "zico") :to-throw))
      (test-it "do not allow &rest operator"
        (expect (macroexpand '(ldef foo (&rest args) nil)) :to-throw)))

    (describe ":rest operator"
      (before-all (ldef rest-fun ((a :number) (b :number) (c :rest)) c))
      (test-it "rest operator works"
        (expect (rest-fun 1 2 3) :to-equal '(3))
        (expect (funcall (rest-fun 1 2) 3 4) :to-equal '(3 4)))

      (test-it ":rest operator can only exist in the final argument"      
        (expect (ldef rest-fail ((c: number) (a :rest) (b :number)) nil) :to-throw))))

  (context "with-l"

    (describe "funcall notation"
      (before-all
        (ldef add3 (x y z) (+ x y z)))
      
      (test-it "works with all arguments at once"
               (expect (with-l (add3 1 2 3)) :to-equal 6))
      
      (test-it "works with partial application - 1 arg then 2"
               (expect (with-l ((add3 1) 2 3)) :to-equal 6))
      
      (test-it "works with partial application - 2 args then 1"
               (expect (with-l ((add3 1 2) 3)) :to-equal 6))
      
      (test-it "works with chained partial applications"
               (expect (with-l (((add3 1) 2) 3)) :to-equal 6))
      
      (test-it "works with full currying chain"
               (expect (with-l ((((add3) 1) 2) 3)) :to-equal 6))

      (describe "regular elisp should work as expected inside with macro"
        (test-it "works with regular arithmetic"
                 (expect (with-l (+ 1 2 3)) :to-equal 6))
        
        (test-it "works with regular list operations"
                 (expect (with-l (car '(1 2 3))) :to-equal 1)
                 (expect (with-l (cdr '(1 2 3))) :to-equal '(2 3)))
        
        (test-it "works with regular function calls"
                 (expect (with-l (length '(1 2 3 4))) :to-equal 4))
        
        (test-it "works with lambda expressions"
                 (expect (with-l (funcall (lambda (x) (+ x 1)) 5)) :to-equal 6))
        
        (test-it "works with let bindings"
                 (expect (with-l (let ((x 10)) (+ x 5))) :to-equal 15))
        
        (test-it "works with if expressions"
                 (expect (with-l (if (> 5 3) "yes" "no")) :to-equal "yes"))
        
        (test-it "works with nested regular expressions"
                 (expect (with-l (+ (* 2 3) (/ 8 2))) :to-equal 10))
        
        (test-it "works with string operations"
                 (expect (with-l (concat "hello" " " "world")) :to-equal "hello world"))
        
        (test-it "works with quoted expressions"
                 (expect (with-l (quote (1 2 3))) :to-equal '(1 2 3)))
        
        (test-it "works with progn"
                 (expect (with-l (progn (+ 1 2) (+ 3 4))) :to-equal 7)))
      )

    (describe "complex transformation scenarios"
      (before-all
        (ldef multiply3 (x y z) (* x y z))
        (ldef subtract2 (x y) (- x y)))

      (test-it "works with nested function calls"
               (expect (with-l (add3 (multiply3 2 3 4) 5 6)) :to-equal 35))

      (test-it "works with multiple curried expressions"
               (expect (with-l (+ ((add3 1) 2 3) ((multiply3 2) 3 4))) :to-equal 30))

      (test-it "works with deeply nested currying"
               (expect (with-l ((((add3) 1) 2) 3)) :to-equal 6))

      (test-it "works with mixed curried and normal calls"
               (expect (with-l (+ (add3 1 2 3) ((subtract2 10) 4))) :to-equal 12))

      (test-it "preserves regular function calls"
               (expect (with-l (+ 1 2 3)) :to-equal 6))

      (test-it "works with lambda expressions"
               (expect (with-l (funcall (lambda (x) (+ x 1)) 5)) :to-equal 6))

      (test-it "works with quoted expressions"
               (expect (with-l (car '(1 2 3))) :to-equal 1))

      (test-it "works with complex nested structures"
               (expect (with-l (list ((add3 1) 2 3) ((multiply3 2 3) 4))) :to-equal '(6 24)))
      
      (test-it "works with nested calls"
        (expect (with-l
                 (with-l
                  (with-l
                   (with-l (* 2 3))))) :to-equal 6)))  
    )

  (context "__"
    (before-all
      (ldef delta (a b c) (- (* b b) (* 4 a c))))

    (test-it "works without currying"
             (expect (delta 1 2 3) :to-equal -8))

    (test-it "works with currying"
             (expect (funcall (delta 1 2) 3) :to-equal -8))

    (test-it "works as a placeholder for first argument"
             (expect (funcall (__ (delta __ 2 3)) 1) :to-equal -8))

    (test-it "works as a placeholder for second argument"
             (expect (funcall (__ (delta 1 __ 3)) 2) :to-equal -8))

    (test-it "works as a placeholder for third argument"
             (expect (funcall (__ (delta 1 2 __)) 3) :to-equal -8))

    (test-it "works without placeholder - behaves like normal currying"
             (expect (funcall (__ (delta 1 2)) 3) :to-equal -8)
             (expect  (__ (delta 1 2 3)) :to-equal -8)))

  (context "l"
    (describe "basic arrow syntax"
      (test-it "creates a lambda with single parameter"
        (let ((square (l x -> (* x x))))
          (expect (funcall square 5) :to-equal 25)))
      
      (test-it "creates a lambda with multiple parameters"
        (let ((add (l x y -> (+ x y))))
          (expect (funcall add 3 4) :to-equal 7)))
      
      (test-it "creates a lambda with multiple body expressions"
        (let ((side-effect-add (l x y -> (message "Adding %s and %s" x y) (+ x y))))
          (expect (funcall side-effect-add 2 3) :to-equal 5)))
      
      (test-it "creates a lambda with no parameters"
        (let ((constant (l -> 42)))
          (expect (funcall constant) :to-equal 42)))
      
      (test-it "works with complex expressions"
        (let ((complex (l x y z -> (+ (* x y) z))))
          (expect (funcall complex 2 3 4) :to-equal 10))))

    (describe "integration with higher-order functions"
      (test-it "works with mapcar"
        (expect (mapcar (l x -> (+ x 1)) '(1 2 3)) :to-equal '(2 3 4)))
      
      (test-it "works with funcall"
        (expect (funcall (l x -> (* x 2)) 5) :to-equal 10))
      
      (test-it "works with apply"
        (expect (apply (l x y -> (+ x y)) '(3 4)) :to-equal 7))
      
      (test-it "works with cl-reduce"
        (expect (cl-reduce (l acc x -> (+ acc x)) '(1 2 3 4) :initial-value 0) :to-equal 10)))

    (describe "integration with lpartial"
      (test-it "works with lpartial"
        (let ((partial-fn (lpartial (l x y z -> (+ x y z)) 1)))
          (expect (funcall partial-fn 2 3) :to-equal 6)))
      
      (test-it "works with chained partial applications"
        (let ((partial-fn (lpartial (l x y z -> (* x y z)) 2 3)))
          (expect (funcall partial-fn 4) :to-equal 24))))

    (describe "integration with with-l"
      (before-all
        (ldef multiply (x y) (* x y)))
      
      (test-it "works inside with-l expressions"
        (expect (with-l (funcall (l x -> (* x 2)) 5)) :to-equal 10))
      
      (test-it "works with curried functions inside with-l"
        (expect (with-l ((l x y -> (+ x y)) 3 4)) :to-equal 7))
      
      (test-it "works with mixed expressions in with-l"
        (expect (with-l (+ (funcall (l x -> (* x 2)) 3) ((multiply 2) 4))) :to-equal 14)))

    (describe "integration with __"
      (test-it "works with placeholder substitution"
        (expect (__ (funcall (l x -> (+ x __)) 5) 3) :to-equal 8))
      
      (test-it "works with multiple placeholders"
        (expect (__ (funcall (l x y -> (+ x y __)) 2 3) 4) :to-equal 9))
      
      (test-it "works with complex placeholder expressions"
        (expect (__ (mapcar (l x -> (+ x __)) '(1 2 3)) 10) :to-equal '(11 12 13))))

    (describe "edge cases and complex scenarios"
      (test-it "works with nested lambda expressions"
        (let ((nested (l x -> (l y -> (+ x y)))))
          (expect (funcall (funcall nested 5) 3) :to-equal 8)))
      
      (test-it "works with conditional expressions"
        (let ((conditional (l x -> (if (> x 0) "positive" "non-positive"))))
          (expect (funcall conditional 5) :to-equal "positive")
          (expect (funcall conditional -1) :to-equal "non-positive")))
      
      (test-it "works with let bindings"
        (let ((with-let (l x y -> (let ((sum (+ x y))) (* sum 2)))))
          (expect (funcall with-let 3 4) :to-equal 14)))           
      
      (test-it "works with list operations"
        (let ((list-processor (l lst -> (cons (car lst) (reverse (cdr lst))))))
          (expect (funcall list-processor '(1 2 3 4)) :to-equal '(1 4 3 2))))
      
      (test-it "works with string operations"
        (let ((string-processor (l str -> (concat str "!!"))))
          (expect (funcall string-processor "Hello") :to-equal "Hello!!")))
      
      (test-it "works with multiple return values simulation"
        (let ((multi-return (l x y -> (list (+ x y) (- x y) (* x y)))))
          (expect (funcall multi-return 10 3) :to-equal '(13 7 30))))
      
      (test-it "works with error handling"
        (let ((safe-divide (l x y -> (if (= y 0) 'division-by-zero (/ x y)))))
          (expect (funcall safe-divide 10 2) :to-equal 5)
          (expect (funcall safe-divide 10 0) :to-equal 'division-by-zero)))
      
      (test-it "works with closure behavior"
        (let* ((counter 0)
               (increment (l -> (setq counter (+ counter 1)))))
          (funcall increment)
          (funcall increment)
          (expect counter :to-equal 2)))

      (test-it "works with with-l"
        (expect (with-l ((l x -> (1+ x)) 10)) :to-equal 11))
      
      (test-it "works with quoted expressions"
        (let ((quoter (l x -> (list 'quote x))))
          (expect (funcall quoter 'hello) :to-equal '(quote hello))))
      
      (test-it "works with backquote and unquote"
        (let ((template (l x y -> `(+ ,x ,y))))
          (expect (funcall template 3 4) :to-equal '(+ 3 4)))))))

;;; l-test.el ends here
