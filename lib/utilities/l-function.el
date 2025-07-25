;; -*- lexical-binding: t; l-syntax: t; -*-

;;; Code:
(require 'l-main)




@doc "`lcomp' provides function composition right to left.

Function composition allows you to combine multiple functions into a single function,
where the output of one function becomes the input of the next. The composition
is applied right to left, meaning the rightmost function is applied first.

Since: NEXT

Arguments:
- No arguments: returns identity function
- One function: returns the function unchanged  
- Two or more functions: returns composed function

Examples:
Basic composition:
\((ldef double (l x -> (* 2 x)))
\((ldef add-one (l x -> (+ 1 x)))
\((ldef composed (lcomp add-one double))
\(composed 5) ;; => (add-one (double 5)) => (add-one 10) => 11

Multiple function composition:
\((lcomp 'inc (l x -> (* 2 x)) (l x -> (* 3 x))) 10) ;; => (inc (* (* 10 3) 2)) ;; 61

Identity cases:
\((lcomp) 42) ;; => 42
\((lcomp 'inc) 5) ;; => 6

String processing example:
\((ldef trim-and-upper (lcomp 'string-upcase 'string-trim))
\(trim-and-upper \"  hello  \") ;; => \"HELLO\"

Mathematical composition:
\((ldef f (l x -> (+ x 1)))
\((ldef g (l x -> (* x 2)))  
\((ldef h (l x -> (- x 3)))
\((lcomp f g h) 10) ;; => (f (g (h 10))) => (f (g 7)) => (f 14) => 15"

 (ldef lcomp () (l x -> x))
 (ldef lcomp ((f :function)) f)
 (ldef lcomp ((f :function) (g :function))
       `(lambda (&rest args) (funcall (quote ,f) (apply (quote ,g) args))))
 (ldef lcomp ((f :function) (g :function) (fn-list :rest))
       (apply 'lcomp (lcomp f g) fn-list))

 



(provide 'l-function)
;;; l-function.el ends here
