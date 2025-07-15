;; -*- lexical-binding: t; l-syntax: t; -*-


;;; Code:

(require 'l-main)


;; @doc "`lcomp' provides function composition right to left.

;; Examples:
;; \((lcomp 'inc (l x -> (* 2 x)) (l x -> (* 3 x))) 10) ;; => (inc (* (* 10 3) 2)) ;; 61"

(ldef lcomp ((f :function)) f)
(ldef lcomp ((f :function) (g nil)) f)
(ldef lcomp ((f :function) (g :function))
      `(lambda (&rest args) (funcall (quote ,f) (apply (quote ,g) args))))
(ldef lcomp ((f :function) (g :function) (fn-list :rest))
      (apply 'lcomp (lcomp f g) fn-list))

((lcomp 'inc) 1)

(provide 'l-function)
;;; l-function.el ends here
(defmacro @doc (docstring &rest ldef-exprs)
  "Add DOCSTRING to FNAME defined with `ldef'."
  `(progn (l-generic-doc ',(cadar ldef-exprs) ,docstring)
          ,@ldef-exprs))
