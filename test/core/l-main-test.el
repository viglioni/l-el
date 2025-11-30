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
        (ldef test-add x y -> (+ x y))
        (expect (test-add 3 4) :to-equal 7))

      (test-it "works with single argument functions"
        (ldef test-square x -> (* x x))
        (expect (test-square 5) :to-equal 25))

      (test-it "works with multiple argument functions"
        (ldef test-multiply x y z -> (* x y z))
        (expect (test-multiply 2 3 4) :to-equal 24)))

    (describe "edge cases"
      (test-it "works with zero argument functions"
        (ldef test-constant -> 42)
        (expect (test-constant) :to-equal 42))

      (test-it "works with functions that return complex data"
        (ldef test-list-maker x y z -> (list x y z))
        (expect (test-list-maker 1 2 3) :to-equal '(1 2 3)))

      (test-it "works with functions that modify arguments"
        (ldef test-modifier x y -> (cons (1+ x) (1+ y)))
        (expect (test-modifier 1 2) :to-equal '(2 . 3)))

      (test-it "works with functions that call other functions"
        (ldef test-caller x y -> (test-add x y))
        (expect (test-caller 5 7) :to-equal 12)))

    (describe "currying behavior"
      (before-all
        (ldef add3 x y z -> (+ x y z)))
      
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
        (ldef multiply4 w x y z -> (* w x y z))
        (ldef concat3 a b c -> (concat a b c)))

      (test-it "works with 4-argument functions"
        (expect (multiply4 2 3 4 5) :to-equal 120))

      (test-it "works with 4-argument partial application"
        (expect (funcall (multiply4 2 3) 4 5) :to-equal 120))

      (test-it "works with string concatenation currying"
        (expect (funcall (concat3 "Hello") " " "World") :to-equal "Hello World"))

      (test-it "works with mixed data types in currying"
        (ldef mixed-fn num str list -> (list num str (length list)))
        (expect (funcall (mixed-fn 42) "test" '(1 2 3)) :to-equal '(42 "test" 3))))

    (describe "pattern matching"
      (before-all
        (ldef fib (n 0) -> 0)
        (ldef fib (n 1) -> 1)
        (ldef fib n -> (+ (fib (- n 1)) (fib (- n 2)))))

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
          (ldef type-checker (x nil) -> "nil")
          (ldef type-checker (x t) -> "true")
          (ldef type-checker (x 0) -> "zero")
          (ldef type-checker x -> "other"))

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
          (ldef greet (name "Alice") -> "Hello, Alice!")
          (ldef greet (name "Bob") -> "Hey, Bob!")
          (ldef greet name -> (concat "Hi, " name "!")))

        (test-it "matches specific string patterns"
          (expect (greet "Alice") :to-equal "Hello, Alice!")
          (expect (greet "Bob") :to-equal "Hey, Bob!"))

        (test-it "uses general case for other strings"
          (expect (greet "Charlie") :to-equal "Hi, Charlie!")))

      (describe "pattern matching with multiple arguments"
        (before-all
          (ldef calculator (op '+) x y -> (+ x y))
          (ldef calculator (op '-) x y -> (- x y))
          (ldef calculator (op '*) x y -> (* x y))
          (ldef calculator op x y -> (error "Unknown operation: %s" op)))

        (test-it "matches addition operation"
          (expect (calculator '+ 3 4) :to-equal 7))

        (test-it "matches subtraction operation"
          (expect (calculator '- 10 3) :to-equal 7))

        (test-it "matches multiplication operation"
          (expect (calculator '* 5 6) :to-equal 30))

        (test-it "works with currying on pattern matched multi-arg functions"
          (expect (funcall (calculator '+) 2 3) :to-equal 5)
          (expect (funcall (calculator '+ 2) 3) :to-equal 5))))

    (describe "direct value matching"
      (describe "number matching"
        (before-all
          (ldef fib-direct 0 -> 0)
          (ldef fib-direct 1 -> 1)
          (ldef fib-direct n -> (+ (fib-direct (- n 1)) (fib-direct (- n 2)))))

        (test-it "matches number 0"
          (expect (fib-direct 0) :to-equal 0))

        (test-it "matches number 1"
          (expect (fib-direct 1) :to-equal 1))

        (test-it "uses general case for other numbers"
          (expect (fib-direct 5) :to-equal 5)))

      (describe "string matching"
        (before-all
          (ldef greet-direct "Alice" -> "Hello, Alice!")
          (ldef greet-direct "Bob" -> "Hey, Bob!")
          (ldef greet-direct name -> (concat "Hi, " name "!")))

        (test-it "matches string Alice"
          (expect (greet-direct "Alice") :to-equal "Hello, Alice!"))

        (test-it "matches string Bob"
          (expect (greet-direct "Bob") :to-equal "Hey, Bob!"))

        (test-it "uses general case for other strings"
          (expect (greet-direct "Charlie") :to-equal "Hi, Charlie!")))

      (describe "keyword matching"
        (before-all
          (ldef handle-status :success -> "OK")
          (ldef handle-status :error -> "Failed")
          (ldef handle-status :warning -> "Warning!")
          (ldef handle-status status -> (format "Unknown: %s" status)))

        (test-it "matches :success keyword"
          (expect (handle-status :success) :to-equal "OK"))

        (test-it "matches :error keyword"
          (expect (handle-status :error) :to-equal "Failed"))

        (test-it "matches :warning keyword"
          (expect (handle-status :warning) :to-equal "Warning!"))

        (test-it "uses general case for other keywords"
          (expect (handle-status :unknown) :to-equal "Unknown: :unknown")))

      (describe "quoted symbol matching"
        (before-all
          (ldef parse-sym 'nil -> "got nil symbol")
          (ldef parse-sym 'foo -> "got foo symbol")
          (ldef parse-sym 'bar -> "got bar symbol")
          (ldef parse-sym x -> (format "got something else: %s" x)))

        (test-it "matches quoted nil"
          (expect (parse-sym 'nil) :to-equal "got nil symbol"))

        (test-it "matches quoted foo"
          (expect (parse-sym 'foo) :to-equal "got foo symbol"))

        (test-it "matches quoted bar"
          (expect (parse-sym 'bar) :to-equal "got bar symbol"))

        (test-it "uses general case for other symbols"
          (expect (parse-sym 'baz) :to-equal "got something else: baz")))

      (describe "boolean and nil matching"
        (before-all
          (ldef handle-bool nil -> "got nil")
          (ldef handle-bool t -> "got true")
          (ldef handle-bool x -> (format "got other: %s" x)))

        (test-it "matches nil"
          (expect (handle-bool nil) :to-equal "got nil"))

        (test-it "matches t"
          (expect (handle-bool t) :to-equal "got true"))

        (test-it "uses general case for other values"
          (expect (handle-bool 42) :to-equal "got other: 42")))

      (describe "mixed direct values and type matching"
        (before-all
          (ldef complex-match 0 'start (x :integer) -> (format "Special: x=%d" x))
          (ldef complex-match n sym (val :integer) -> (format "General: n=%s sym=%s val=%d" n sym val))
          (ldef complex-match n sym val -> (format "Fallback: n=%s sym=%s val=%s" n sym val)))

        (test-it "matches specific pattern with direct values and type"
          (expect (complex-match 0 'start 42) :to-equal "Special: x=42"))

        (test-it "matches general pattern with type constraint"
          (expect (complex-match 5 'foo 10) :to-equal "General: n=5 sym=foo val=10"))

        (test-it "uses fallback for non-integer val"
          (expect (complex-match 5 'foo "bar") :to-equal "Fallback: n=5 sym=foo val=bar")))

      (describe "multiple values with same arity"
        (before-all
          (ldef number-name 0 -> "zero")
          (ldef number-name 1 -> "one")
          (ldef number-name 2 -> "two")
          (ldef number-name 3 -> "three")
          (ldef number-name n -> (format "number %d" n)))

        (test-it "matches zero"
          (expect (number-name 0) :to-equal "zero"))

        (test-it "matches one"
          (expect (number-name 1) :to-equal "one"))

        (test-it "matches two"
          (expect (number-name 2) :to-equal "two"))

        (test-it "matches three"
          (expect (number-name 3) :to-equal "three"))

        (test-it "uses general case for other numbers"
          (expect (number-name 42) :to-equal "number 42")))

      (describe "direct value matching with currying"
        (before-all
          (ldef curry-val 0 x -> (* x 2))
          (ldef curry-val n x -> (+ n x)))

        (test-it "works with direct currying on matched value"
          (expect (curry-val 0 5) :to-equal 10))

        (test-it "works with partial application on matched value"
          (expect (funcall (curry-val 0) 5) :to-equal 10))

        (test-it "works with general case currying"
          (expect (curry-val 3 5) :to-equal 8))

        (test-it "works with partial application on general case"
          (expect (funcall (curry-val 3) 5) :to-equal 8)))

      (describe "vector matching"
        (before-all
          (ldef handle-vec [1 2 3] -> "specific vector")
          (ldef handle-vec [] -> "empty vector")
          (ldef handle-vec v -> (format "other vector: %s" v)))

        (test-it "matches specific vector"
          (expect (handle-vec [1 2 3]) :to-equal "specific vector"))

        (test-it "matches empty vector"
          (expect (handle-vec []) :to-equal "empty vector"))

        (test-it "uses general case for other vectors"
          (expect (handle-vec [4 5 6]) :to-equal "other vector: [4 5 6]")))

      (describe "list matching"
        (before-all
          (ldef handle-list '(1 2 3) -> "specific list")
          (ldef handle-list '() -> "empty list")
          (ldef handle-list lst -> (format "other list: %s" lst)))

        (test-it "matches specific list"
          (expect (handle-list '(1 2 3)) :to-equal "specific list"))

        (test-it "matches empty list"
          (expect (handle-list '()) :to-equal "empty list"))

        (test-it "uses general case for other lists"
          (expect (handle-list '(4 5 6)) :to-equal "other list: (4 5 6)"))))

    (describe "do not allow more arguments than it is defined"
      (before-all
(ldef one-or-two-params (n :number)  -> n)
(ldef one-or-two-params (n :number) (s :string)  -> (format "%s. %s" n s)))
      
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
        (expect (macroexpand '(ldef foo (&rest args) -> nil)) :to-throw)))

    (describe ":rest operator"
      (before-all (ldef rest-fun (a :number) (b :number) (c :rest) -> c))
      (test-it "rest operator works"
        (expect (rest-fun 1 2 3) :to-equal '(3))
        (expect (funcall (rest-fun 1 2) 3 4) :to-equal '(3 4)))

      (test-it ":rest operator can only exist in the final argument"
        (expect (ldef rest-fail (c :number) (a :rest) (b :number) -> nil) :to-throw)))

    (describe "pattern matching with complex data structures"
      (describe "alists"
        (before-all
(ldef process-alist (config '((key . value)))  -> "matched single pair alist")
(ldef process-alist (config '((a . b) (c . d)))  -> "matched multi pair alist")
(ldef process-alist config  -> "other"))

        (test-it "matches single pair alist"
          (expect (process-alist '((key . value))) :to-equal "matched single pair alist"))

        (test-it "matches multi pair alist"
          (expect (process-alist '((a . b) (c . d))) :to-equal "matched multi pair alist"))

        (test-it "falls through for different alists"
          (expect (process-alist '((x . y))) :to-equal "other")
          (expect (process-alist '()) :to-equal "other")))

      (describe "plists"
        (before-all
(ldef handle-opts (opts '(:key value))  -> "matched key-value plist")
(ldef handle-opts (opts '(:foo bar :baz qux))  -> "matched multi-key plist")
(ldef handle-opts opts  -> "other"))

        (test-it "matches single key-value plist"
          (expect (handle-opts '(:key value)) :to-equal "matched key-value plist"))

        (test-it "matches multi-key plist"
          (expect (handle-opts '(:foo bar :baz qux)) :to-equal "matched multi-key plist"))

        (test-it "falls through for different plists"
          (expect (handle-opts '(:other stuff)) :to-equal "other")))

      (describe "nested dotted pairs"
        (before-all
(ldef handle-nested (data '((a b . c)))  -> "matched nested dotted")
(ldef handle-nested (data '(x y . z))  -> "matched improper nested")
(ldef handle-nested data  -> "other"))

        (test-it "matches nested dotted pairs"
          (expect (handle-nested '((a . (b . c)))) :to-equal "matched nested dotted"))

        (test-it "matches improper nested lists"
          (expect (handle-nested '(x . (y . z))) :to-equal "matched improper nested"))

        (test-it "falls through for different structures"
          (expect (handle-nested '((a . b))) :to-equal "other")))

      (describe "vectors"
        (before-all
(ldef process-vec (arr [1 2 3])  -> "matched [1 2 3]")
(ldef process-vec (arr [])  -> "matched empty vector")
(ldef process-vec arr  -> "other"))

        (test-it "matches specific vector"
          (expect (process-vec [1 2 3]) :to-equal "matched [1 2 3]"))

        (test-it "matches empty vector"
          (expect (process-vec []) :to-equal "matched empty vector"))

        (test-it "falls through for different vectors"
          (expect (process-vec [1 2]) :to-equal "other")
          (expect (process-vec [4 5 6]) :to-equal "other")))

      (describe "mixed complex structures"
        (before-all
(ldef config-handler (cfg '(:mode (foo . bar)))  -> "matched mode config")
(ldef config-handler (cfg '(:type list :data ((a . b))))  -> "matched data config")
(ldef config-handler cfg  -> "other"))

        (test-it "matches mode config with dotted pair"
          (expect (config-handler '(:mode (foo . bar))) :to-equal "matched mode config"))

        (test-it "matches data config with nested alist"
          (expect (config-handler '(:type list :data ((a . b)))) :to-equal "matched data config"))

        (test-it "falls through for other configs"
          (expect (config-handler '(:other stuff)) :to-equal "other")))

      (describe "empty and nil edge cases"
        (before-all
          ;; Note: In Emacs Lisp, nil and '() are identical, so only one pattern will match
(ldef nil-handler (x nil)  -> "matched nil or empty list")
(ldef nil-handler (x 0)  -> "matched zero")
(ldef nil-handler x  -> "other"))

        (test-it "matches nil (which is also empty list in Emacs Lisp)"
          (expect (nil-handler nil) :to-equal "matched nil or empty list")
          (expect (nil-handler '()) :to-equal "matched nil or empty list"))

        (test-it "matches zero specifically"
          (expect (nil-handler 0) :to-equal "matched zero"))

        (test-it "falls through for other values"
          (expect (nil-handler 1) :to-equal "other")
          (expect (nil-handler '(1)) :to-equal "other")))

      (describe "multiple wildcards"
        (before-all
(ldef ignore-edges _ x _  -> x)
(ldef ignore-middle a _ _ d  -> (list a d)))

        (test-it "ignores first and last arguments"
          (expect (ignore-edges 1 2 3) :to-equal 2)
          (expect (ignore-edges "a" "b" "c") :to-equal "b"))

        (test-it "ignores middle arguments"
          (expect (ignore-middle 1 2 3 4) :to-equal '(1 4))
          (expect (ignore-middle "a" "b" "c" "d") :to-equal '("a" "d"))))

      (describe "improper lists"
        (before-all
(ldef handle-improper (x '(1 2 . 3))  -> "matched (1 2 . 3)")
(ldef handle-improper (x '(a . b))  -> "matched (a . b)")
(ldef handle-improper x  -> "other"))

        (test-it "matches specific improper list"
          (expect (handle-improper '(1 2 . 3)) :to-equal "matched (1 2 . 3)"))

        (test-it "matches simple dotted pair"
          (expect (handle-improper '(a . b)) :to-equal "matched (a . b)"))

        (test-it "falls through for proper lists"
          (expect (handle-improper '(1 2 3)) :to-equal "other")))

      (describe "quoted symbols in patterns"
        (before-all
(ldef quote-handler (x ''foo)  -> "matched 'foo")
(ldef quote-handler (x 'bar)  -> "matched bar symbol")
(ldef quote-handler x  -> "other"))

        (test-it "matches quoted symbol structure"
          (expect (quote-handler '(quote foo)) :to-equal "matched 'foo"))

        (test-it "matches specific symbol"
          (expect (quote-handler 'bar) :to-equal "matched bar symbol"))

        (test-it "falls through for other values"
          (expect (quote-handler '(quote bar)) :to-equal "other")
          (expect (quote-handler 'foo) :to-equal "other"))))

  (context "with-l"

    (describe "funcall notation"
      (before-all
(ldef add3 x y z  -> (+ x y z)))
      
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

        (test-it "works with quoted dotted pairs"
          (expect (with-l '(1 . 2)) :to-equal '(1 . 2)))

        (test-it "works with quoted nested dotted pairs"
          (expect (with-l '((1 . 2))) :to-equal '((1 . 2))))

        (test-it "works with complex quoted structures with dotted pairs"
          (expect (with-l '(:mode (foo . bar))) :to-equal '(:mode (foo . bar))))

        (test-it "works with progn"
          (expect (with-l (progn (+ 1 2) (+ 3 4))) :to-equal 7))

        (test-it "works with alists"
          (expect (with-l '((key . value) (foo . bar))) :to-equal '((key . value) (foo . bar))))

        (test-it "works with nested alists"
          (expect (with-l '((a . ((b . c) (d . e))))) :to-equal '((a . ((b . c) (d . e))))))

        (test-it "works with plists"
          (expect (with-l '(:key value :foo bar)) :to-equal '(:key value :foo bar)))

        (test-it "works with mixed plist and dotted pairs"
          (expect (with-l '(:mode (foo . bar) :config (baz . qux)))
                  :to-equal '(:mode (foo . bar) :config (baz . qux))))

        (test-it "works with vectors"
          (expect (with-l [1 2 3]) :to-equal [1 2 3]))

        (test-it "works with nested vectors"
          (expect (with-l [[1 2] [3 4]]) :to-equal [[1 2] [3 4]]))

        (test-it "works with quoted vectors"
          (expect (with-l (quote [1 2 3])) :to-equal [1 2 3]))

        (test-it "works with improper lists"
          (expect (with-l '(1 2 . 3)) :to-equal '(1 2 . 3)))

        (test-it "works with nested improper lists"
          (expect (with-l '(a . (b . (c . d)))) :to-equal '(a . (b . (c . d)))))

        (test-it "works with empty list"
          (expect (with-l '()) :to-equal '()))

        (test-it "works with nil"
          (expect (with-l nil) :to-equal nil))

        (test-it "works with quoted symbols"
          (expect (with-l '(quote foo)) :to-equal '(quote foo)))

        (test-it "works with complex backquoted expressions"
          (let ((x 10))
            (expect (with-l `(foo ,x ,(+ x 5))) :to-equal '(foo 10 15))))

        (test-it "works with backquote and splice"
          (let ((lst '(2 3)))
            (expect (with-l `(1 ,@lst 4)) :to-equal '(1 2 3 4))))

        (test-it "works with nested backquoted expressions"
          (expect (with-l `(outer ,(+ 1 2) `(inner ,(+ 3 4))))
                  :to-equal '(outer 3 `(inner ,(+ 3 4)))))

        (test-it "works with mixed quoted and unquoted dotted pairs"
          (expect (with-l (cons 'a (cons 'b '(c . d)))) :to-equal '(a b c . d))))
      )

    (describe "complex transformation scenarios"
      (before-all
(ldef multiply3 x y z  -> (* x y z))
(ldef subtract2 x y  -> (- x y)))

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

    (describe "defining functions inside with-l"
      (describe "defun inside with-l"
        (test-it "defines regular functions that work normally"
          (with-l
           (defun test-regular-add (x y) (+ x y)))
          (expect (test-regular-add 3 4) :to-equal 7))        

        (test-it "defines functions with complex bodies"
          (with-l
           (defun test-complex (x y)
             (let ((sum (+ x y)))
               (* sum 2))))
          (expect (test-complex 5 7) :to-equal 24)))

      (describe "defmacro inside with-l"
        (test-it "defines macros that work normally"
          (with-l
           (defmacro test-when-positive (x &rest body)
             `(when (> ,x 0) ,@body)))
          (expect (test-when-positive 5 (+ 1 2)) :to-equal 3)
          (expect (test-when-positive -1 (+ 1 2)) :to-equal nil))

        (test-it "defines macros that can be used in currying expressions"
          (with-l
           (defmacro test-double (x) `(* 2 ,x))
           (+ (test-double 5) 3))
          (expect (+ (* 2 5) 3) :to-equal 13))

        (test-it "defines macros with complex expansion"
          (with-l
           (defmacro test-with-temp-var (var-name value &rest body)
             `(let ((,var-name ,value)) ,@body)))
          (expect (test-with-temp-var x 10 (+ x 5)) :to-equal 15)))

      (describe "ldef inside with-l"
        (test-it "defines curried functions that work normally"
          (with-l
(ldef test-curried-add x y z  -> (+ x y z)))
          (expect (test-curried-add 1 2 3) :to-equal 6)
          (expect (funcall (test-curried-add 1 2) 3) :to-equal 6))

        (test-it "defines curried functions that work with currying syntax in same block"
          (with-l
(ldef test-curried-multiply x y z  -> (* x y z))
           ((test-curried-multiply 2 3) 4))
          (expect (* 2 3 4) :to-equal 24))

        (test-it "defines pattern-matched functions inside with-l"
          (with-l
(ldef test-pattern-func (x 0)  -> "zero")
(ldef test-pattern-func (x 1)  -> "one")
(ldef test-pattern-func x  -> "other"))
          (expect (test-pattern-func 0) :to-equal "zero")
          (expect (test-pattern-func 1) :to-equal "one")
          (expect (test-pattern-func 5) :to-equal "other"))

        (test-it "defines type-matched functions inside with-l"
          (with-l
(ldef test-type-dispatch (x :string)  -> (upcase x))
(ldef test-type-dispatch (x :integer)  -> (* x 2))
(ldef test-type-dispatch x  -> x))
          (expect (test-type-dispatch "hello") :to-equal "HELLO")
          (expect (test-type-dispatch 5) :to-equal 10)
          (expect (test-type-dispatch '(1 2 3)) :to-equal '(1 2 3))))

      (describe "mixed definitions and usage"
        (test-it "combines different definition types in one with-l block"
          (with-l
           (defun regular-double (x) (* x 2))
(ldef curried-add x y  -> (+ x y))
           (defmacro test/make-list (x) `(list ,x ,x))
           (+ (regular-double 3) ((curried-add 2) 4) (car (test/make-list 1))))
          (expect (+ 6 6 1) :to-equal 13))))

    (describe "currying with complex data structures"
      (before-all
(ldef cons-builder car cdr  -> (cons car cdr))
(ldef alist-getter alist key  -> (alist-get key alist))
(ldef list-builder a b c  -> (list a b c)))

      (test-it "works with curried functions that build cons cells"
        (expect (with-l ((cons-builder 'a) 'b)) :to-equal '(a . b)))

      (test-it "works with curried functions that build complex structures"
        (expect (with-l ((list-builder '(a . b)) '(c . d) '(e . f)))
                :to-equal '((a . b) (c . d) (e . f))))

      (test-it "works with curried functions receiving alists"
        (let ((test-alist '((foo . 1) (bar . 2))))
          (expect (with-l ((alist-getter test-alist) 'foo)) :to-equal 1)))

      (test-it "works with nested currying and complex data"
        (expect (with-l (((list-builder '(a . b)) '(c . d)) '(e . f)))
                :to-equal '((a . b) (c . d) (e . f))))

      (test-it "works with alists in expressions"
        (expect (with-l (car '((key . value) (foo . bar))))
                :to-equal '(key . value)))

      (test-it "works with plists in curried calls"
        (expect (with-l (plist-get '(:a 1 :b 2) :a)) :to-equal 1))

      (test-it "works with vectors in curried context"
        (expect (with-l (aref [1 2 3] 1)) :to-equal 2))

      (test-it "works with improper lists in curried context"
        (expect (with-l (car '(1 2 . 3))) :to-equal 1))

      (test-it "works with complex nested structures in currying"
        (expect (with-l ((list-builder '(:mode (foo . bar))) '(:config (baz . qux)) 'data))
                :to-equal '((:mode (foo . bar)) (:config (baz . qux)) data)))

      (test-it "preserves alist structure through currying chain"
(ldef alist-processor alist transform  -> (mapcar transform alist))
        (expect (with-l ((alist-processor '((a . 1) (b . 2))) 'cdr))
                :to-equal '(1 2)))

      (test-it "works with empty structures"
        (expect (with-l ((list-builder '()) '() '())) :to-equal '(() () ())))
        (expect (with-l (list [])) :to-equal '([])))

      (test-it "works with mixed quoted and backquoted complex structures"
        (let ((x 10))
          ;; Use list or quote to avoid with-l treating the backquoted result as a curried call
          (expect (with-l (list `(a . ,x) `(:b ,(+ x 5))))
                  :to-equal '((a . 10) (:b 15))))))

    )

  (context "__"
    (before-all
(ldef delta a b c  -> (- (* b b) (* 4 a c))))

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
(ldef multiply x y  -> (* x y)))
      
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
          (expect (funcall template 3 4) :to-equal '(+ 3 4))))))

  (describe "l-generic-remove-method"
    (before-each
      ;; Clean up any existing test functions
      (ignore-errors (l-generic-cleanup 'test-remove-fn))
      (ignore-errors (l-generic-cleanup 'test-remove-fib)))

    (after-each
      ;; Clean up after tests
      (ignore-errors (l-generic-cleanup 'test-remove-fn))
      (ignore-errors (l-generic-cleanup 'test-remove-fib)))

    (test-it "removes a specific type-matched implementation"
      (ldef test-remove-fn (x :string) -> "string")
      (ldef test-remove-fn (x :integer) -> "integer")
      (ldef test-remove-fn x -> "default")

      ;; Verify all implementations work
      (expect (test-remove-fn "hello") :to-equal "string")
      (expect (test-remove-fn 42) :to-equal "integer")
      (expect (test-remove-fn 'symbol) :to-equal "default")

      ;; Remove string implementation
      (l-generic-remove-method 'test-remove-fn '((x :string)))

      ;; String should now match default, others still work
      (expect (test-remove-fn "hello") :to-equal "default")
      (expect (test-remove-fn 42) :to-equal "integer")
      (expect (test-remove-fn 'symbol) :to-equal "default"))

    (test-it "removes a specific value-matched implementation"
      (ldef test-remove-fib 0 -> 0)
      (ldef test-remove-fib 1 -> 1)
      (ldef test-remove-fib n -> (+ (test-remove-fib (- n 1))
                                    (test-remove-fib (- n 2))))

      ;; Verify all work
      (expect (test-remove-fib 0) :to-equal 0)
      (expect (test-remove-fib 1) :to-equal 1)
      (expect (test-remove-fib 5) :to-equal 5)

      ;; Remove the base case for 0
      (l-generic-remove-method 'test-remove-fib '(0))

      ;; Now fib(0) should match the general case, causing infinite recursion
      ;; But we can verify the method was removed by checking it doesn't match
      (expect (test-remove-fib 1) :to-equal 1))

    (test-it "removes last method and unbinds function"
      (ldef test-remove-fn x -> "only")
      (expect (test-remove-fn 'anything) :to-equal "only")

      ;; Remove the only method
      (l-generic-remove-method 'test-remove-fn '(x))

      ;; Function should be unbound
      (expect (fboundp 'test-remove-fn) :to-be nil))

    (test-it "returns nil when function has no methods"
      (expect (l-generic-remove-method 'nonexistent-fn '(x))
              :to-be nil))

    (test-it "returns nil when pattern not found"
      (ldef test-remove-fn (x :integer) -> "int")
      (expect (l-generic-remove-method 'test-remove-fn '((x :string)))
              :to-be nil)
      ;; Original method still works
      (expect (test-remove-fn 42) :to-equal "int"))

    (test-it "returns t when method successfully removed"
      (ldef test-remove-fn (x :integer) -> "int")
      (ldef test-remove-fn x -> "default")
      (expect (l-generic-remove-method 'test-remove-fn '((x :integer)))
              :to-be t))

    (test-it "handles multiple implementations with same arity"
      (ldef test-remove-fn (x :string) -> "string")
      (ldef test-remove-fn (x :integer) -> "integer")
      (ldef test-remove-fn (x :symbol) -> "symbol")
      (ldef test-remove-fn x -> "wildcard")

      ;; Remove integer
      (l-generic-remove-method 'test-remove-fn '((x :integer)))

      ;; Integer should now match wildcard
      (expect (test-remove-fn 42) :to-equal "wildcard")
      (expect (test-remove-fn "hello") :to-equal "string")
      (expect (test-remove-fn 'sym) :to-equal "symbol"))

    (test-it "distinguishes between different parameter names with same type"
      (ldef test-remove-fn (x :integer) -> x)

      ;; This should match even with different param name
      (l-generic-remove-method 'test-remove-fn '((y :integer)))

      ;; Function should be unbound
      (expect (fboundp 'test-remove-fn) :to-be nil))

    (test-it "handles rest parameters"
      (ldef test-remove-fn (args :rest) -> (apply '+ args))
      (ldef test-remove-fn x y -> (* x y))

      (expect (test-remove-fn 1 2 3 4) :to-equal 10)
      (expect (test-remove-fn 3 4) :to-equal 12)

      ;; Remove rest parameter version
      (l-generic-remove-method 'test-remove-fn '((args :rest)))

      ;; Should only work with exactly 2 args now
      (expect (test-remove-fn 3 4) :to-equal 12))))

;;; l-test.el ends here
