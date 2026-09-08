;;; l-main.el --- Modern functional programming utilities for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Laura Viglioni

;; Author: Laura Viglioni
;; Keywords: lisp, functional, programming, utilities
;; URL: https://github.com/viglioni/l-el
;; since: 0.2.0
;; updated-at: (0.3.0 0.3.3 0.4.0 1.0.0 1.1.0 1.2.0)

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; l.el provides a modern functional programming approach to writing
;; Emacs Lisp, drawing inspiration from Common Lisp, Haskell and Elixir.
;;
;; This library introduces currying, partial application, pattern matching,
;; arrow-syntax lambdas, and placeholder substitution utilities that make
;; Emacs Lisp more expressive and closer to modern functional programming
;; languages.
;;
;; Key features:
;; - Automatic currying with `ldef'
;; - Pattern matching with `ldef'
;; - Type matching with `ldef'
;; - Arrow-syntax lambdas: `l' (anonymous) and `li' (interactive)
;; - Partial application with `lpartial'
;; - Placeholder substitution with `__'
;; - Curried-call syntax via `with-l'
;; - Attach docstrings to `ldef' functions with `@doc'
;; - Check whether a symbol is an `ldef' with `ldefp'
;; - Optional buffer-wide syntax transformation via `l-syntax'
;;
;; Example usage:
;;
;;   (ldef add3 x y z -> (+ x y z))
;;   (funcall (add3 1 2) 3) ; => 6
;;
;;   (ldef greet "Alice" -> "Hello, Alice!")
;;   (ldef greet name -> (concat "Hi, " name "!"))
;;   (greet "Alice") ; => "Hello, Alice!"
;;   (greet "Bob")   ; => "Hi, Bob!"
;;
;;   (mapcar (l x -> (* x 2)) '(1 2 3)) ; => (2 4 6)
;;
;;   (global-set-key (kbd "C-c h") (li -> (message "hi")))
;;
;;   (with-l
;;     ((add3 1) 2 3)) ; => 6
;;
;;   (__ (+ __ (* __ 2)) 5) ; => 15
;;
;;   (funcall (lpartial '+ 10) 5) ; => 15

;;; Code:

(require 'cl-lib)
(require 'l-generic)
(require 'l-exception)

(defgroup l nil
  "Modern functional programming utilities for Emacs Lisp."
  :group 'lisp
  :prefix "l-"
  :link '(url-link "https://github.com/viglioni/l-el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro-expansion-time helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro l--with-arrow (arrow-expr &rest body)
  "Bind ARROW-POS, ARROW-EXPR-ARGS, and ARROW-EXPR-BODY around BODY.

Splits ARROW-EXPR (which must evaluate to a list at runtime) around the
symbol `->':

  ARROW-POS        - index of `->' in the list.
  ARROW-EXPR-ARGS  - sublist before `->'.
  ARROW-EXPR-BODY  - sublist after `->'.

BODY is spliced into the expansion with those bindings in scope.

Signals `l-invalid-syntax-error' if `->' is absent.

ARROW-EXPR must be a *symbol* naming a list (typically an `&rest'
parameter of the calling macro) or a quoted literal — never an unquoted
list literal, which would be evaluated as a function call.

Used by `l' and `ldef' to parse arrow-syntax argument lists.

since: 1.2.0"
  (declare (indent defun))
  `(let* ((arrow-pos (or (cl-position '-> ,arrow-expr)
                         (signal 'l-invalid-syntax-error
                                 (list "Expected `->' separator in expression"))))
          (arrow-expr-args (cl-subseq ,arrow-expr 0 arrow-pos))
          (arrow-expr-body (cl-subseq ,arrow-expr (1+ arrow-pos))))
     ,@body))

;;;;;;;;;
;; API ;;
;;;;;;;;;

(defun lpartial (fn &rest init-args)
  "Return a partially applied function with FN and INIT-ARGS.

Creates a new function that, when called, applies FN to the
combination of INIT-ARGS (provided now) and any additional
arguments (provided later).

Examples:
  (funcall (lpartial \\='+ 5) 3)     ;; => 8
  (funcall (lpartial \\='* 2 3) 4)   ;; => 24
  (funcall (lpartial \\='concat \"Hello, \") \"World!\")
  ;; => \"Hello, World!\"

FN can be a function symbol, lambda expression, or any callable.
INIT-ARGS are the initial arguments to partially apply to FN.

since: 0.2.0"
  (lambda (&rest args)
    (apply fn (append init-args args))))

(defmacro ldef (name &rest args-and-body)
  "Define autocurried functions with pattern matching support.

Creates a function NAME that automatically curries when called with fewer
arguments and supports pattern matching on arguments.

NEW SYNTAX (using -> separator):
  (ldef name arg1 arg2 ... -> body...)

Arguments before -> are parameter patterns supporting:
- Regular parameters: x, y, z
- Type matches: (x :integer), (y :string), etc.
- Value matches: (x 0), (y \"value\"), etc.
- Wildcards: _ignore, _var (bind but conventionally ignore)

Methods are ordered by specificity (lexicographically):
- Value matches: \"d\" (most specific)
- Parameterized types: \"c\"
- Primitive types: \"b\"
- Category types: \"a\"
- Wildcards: \"0\" (least specific)

PATTERN MATCHING:
Arguments support direct value matching and type matching.
- Symbol: x - matches any value, binds to x
- Wildcard: _ignore - matches any value, binds but conventionally ignored
- Direct value: 0, \"Alice\", :success, 'foo - matches when arg equals value
- Type match: (x :integer) - matches only when x satisfies integerp
- List in pattern: (...) - always indicates type matching or rest parameter

Pattern matching examples:
  (ldef fib 0 -> 0)                        ;; matches when arg = 0
  (ldef fib 1 -> 1)                        ;; matches when arg = 1
  (ldef fib n -> (+ (fib (- n 1)) (fib (- n 2))))  ;; general case

  (ldef greet \"Alice\" -> \"Hello, Alice!\")      ;; matches \"Alice\"
  (ldef greet name -> (concat \"Hi, \" name \"!\"))   ;; general case

  (ldef calc '+ x y -> (+ x y))            ;; matches when op = '+
  (ldef calc '* x y -> (* x y))            ;; matches when op = '*
  (ldef calc _op _x _y -> (error \"Unknown operation\"))  ;; fallback

CURRYING:
Functions defined with ldef automatically curry when called
with fewer arguments.

Currying examples:
  (ldef add3 x y z -> (+ x y z))
  (add3 1 2 3)        ;; => 6 (full application)
  (funcall (add3 1) 2 3)  ;; => 6 (partial application)
  (funcall (funcall (add3 1) 2) 3)  ;; => 6 (chained partial)

MANAGING IMPLEMENTATIONS:
- Use \\='l-generic-cleanup\\=' to remove all methods of a function.
- Use \\='l-generic-remove-method\\=' to remove a specific implementation
  while keeping others.

NAME is the function name to define.
ARGS-AND-BODY contains arguments, ->, and body expressions.

since: 1.0.0"

  (l--with-arrow args-and-body
    ;; Check for &rest
    (when (cl-some (lambda (arg)
                     (or (eq arg '&rest)
                         (and (listp arg) (memq '&rest arg))))
                   arrow-expr-args)
      (signal 'l-invalid-rest-parameter-error
              (list name "Use (param :rest) instead of &rest")))
    
    `(l-generic ,name ,arrow-expr-args ,@arrow-expr-body)))

(defmacro with-l (&rest body)
  "Transform expressions to support curried function call syntax.

Enables the use of ((fn args) more-args) syntax within the macro body,
transforming such expressions into proper funcall forms.
This allows for more natural curried function composition and chaining.

The transformation converts:
  ((fn arg1) arg2 arg3)  =>  (funcall (fn arg1) arg2 arg3)
  (((fn arg1) arg2) arg3)  =>  (funcall (funcall (fn arg1) arg2) arg3)

Examples:
  (with-l ((add3 1) 2 3))     ;; => 6
  (with-l (((add3 1) 2) 3))   ;; => 6
  (with-l (+ ((add3 1) 2 3) ((multiply3 2) 3 4)))  ;; => 30

BODY contains the expressions to transform.
Regular function calls and other expressions are left unchanged.

since: 0.2.0"
  (let ((grouped-body (l--group-doc-expressions body)))
    `(progn ,@(mapcar (lambda (expr)
                        (l--transform-curry-calls (macroexpand-all expr)))
                      grouped-body))))

(defmacro l (&rest expr)
  "Lambda macro for creating functions with arrow syntax.

This macro provides a more concise way to create lambda functions
using arrow syntax inspired by other functional programming languages.
The arrow `->` separates the parameter list from the function body.

EXPR is a list of expressions where `->` acts as a separator between
the parameter list and the function body.

Syntax:
  (l param1 param2 ... -> body-expr1 body-expr2 ...)

This is equivalent to:
  (lambda (param1 param2 ...) body-expr1 body-expr2 ...)

Examples:
  (l x y -> (+ x y))
  ;; Equivalent to: (lambda (x y) (+ x y))

  (l x -> (* x x))
  ;; Equivalent to: (lambda (x) (* x x))

  (l x y -> (message \"Adding %s and %s\" x y) (+ x y))
  ;; Equivalent to: (lambda (x y) (message \"Adding %s and %s\" x y) (+ x y))

  (funcall (l x -> (* x 2)) 5)
  ;; => 10

  (mapcar (l x -> (+ x 1)) '(1 2 3))
  ;; => (2 3 4)

The arrow `->` must be present in the expression list, otherwise
the macro will not work correctly.

since: 0.2.0"
  (l--with-arrow expr
    `(lambda ,arrow-expr-args ,@arrow-expr-body)))

(defmacro li (&rest expr)
  "Lambda macro for creating interactive functions with arrow syntax.

Like `l', but injects `(interactive)' as the first form of the produced
lambda so the result can be used directly as an interactive command
(e.g. bound to a key or invoked via \\[execute-extended-command]).

EXPR is a list of expressions where `->' separates the parameter list
from the body.

Syntax:
  (li param1 param2 ... -> body-expr1 body-expr2 ...)

Equivalent to:
  (lambda (param1 param2 ...) (interactive) body-expr1 body-expr2 ...)

Examples:
  (global-set-key (kbd \"C-c h\") (li -> (message \"hi\")))

  (funcall (li -> (message \"called\")))
  ;; prints \"called\"

  (li x -> (message \"got %s\" x))
  ;; => (lambda (x) (interactive) (message \"got %s\" x))

Signals `l-invalid-syntax-error' if `->' is absent.

since: 1.2.0"
  (l--with-arrow expr
    `(lambda ,arrow-expr-args (interactive) ,@arrow-expr-body)))

(defmacro __ (block &optional arg)
  "Substitute all occurrences of \\=`__\\=' in BLOCK with ARG.

This macro provides a convenient way to create expressions
with placeholder substitution.
Every occurrence of the symbol \\=`__\\=' in BLOCK
will be replaced with ARG before evaluation.

BLOCK is the expression containing placeholder symbols \\=`__\\='.
ARG is the value that will replace all \\=`__\\=' placeholders.
If not provided, returns a function that expects one argument.

Example:
  (__ (+ __ (* __ 2)) 5)
  ;; Expands to: (+ 5 (* 5 2))
  ;; Evaluates to: 15

  (__ (+ __ (* __ 2)))
  ;; Returns a function that expects one argument
  ;; (funcall (__ (+ __ (* __ 2))) 5) evaluates to: 15

  (__ (list __ (car __) (cdr __)) \\='(1 2 3))
  ;; Expands to: (list (1 2 3) (car (1 2 3)) (cdr (1 2 3)))
  ;; Evaluates to: ((1 2 3) 1 (2 3))

The substitution is recursive, so nested lists and complex
expressions are handled correctly.

since: 0.2.0"
  (cl-labels ((substitute-__ (expr replacement)
                (cond
                 ((eq expr '__) replacement)
                 ((listp expr) (mapcar (lambda (x) (substitute-__ x replacement)) expr))
                 (t expr)))
              (has-__ (expr)
                (cond
                 ((eq expr '__) t)
                 ((listp expr) (cl-some #'has-__ expr))
                 (t nil))))
    (if (has-__ block)
        (if arg
            (substitute-__ block arg)
          `(lambda (x) ,(substitute-__ block 'x)))
      block)))

(defmacro @doc (docstring &rest ldef-exprs)
  "Add DOCSTRING to function-name defined in LDEF-EXPRS defined with `ldef'.

since: 0.3.0"
  `(progn (l-generic-doc ',(cadar ldef-exprs) ,docstring)
          ,@ldef-exprs))

(defun ldefp (name)
  "Return non-nil if NAME is a function defined with ldef.
Checks if NAME has entries in the generic function registry.

since: 1.1.0

Examples:
  (ldef my-func x -> (+ x 1))
  (ldefp 'my-func)  ;; => t
  (ldefp 'concat)   ;; => nil (built-in function)
  (ldefp 'when)     ;; => nil (macro)"
  (not (null (l--get-from-registry name))))


;;;;;;;;;;;;;;;;;;;;;;;
;; Private functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun l--transform-curry-calls (expr)
  "Transform curry call expressions recursively.

Internal function used by with-l to transform expressions
containing curried function calls.
Identifies patterns like ((fn args) more-args) and
converts them to (funcall (fn args) more-args).

The transformation rules:
- ((fn args) more-args) becomes (funcall (fn args) more-args)
- Lambda expressions are preserved as-is
- Special forms (ldef, defun, etc.) are preserved as-is
- Regular expressions are recursively processed
- Atoms are left unchanged

EXPR is the expression to transform, can be an atom, list, or nested structure.
Returns the transformed expression with curried calls
converted to funcall forms.

since: 0.2.0"
  (cond
   ;; Don't transform special forms
   ((and (consp expr)
         (symbolp (car expr))
         (memq (car expr) '(ldef defun defmacro defvar defcustom
                           lambda quote function let let* progn
                           if when unless cond case)))
    ;; For special forms, only recursively transform their body parts where appropriate
    (cond
      ;; quote and function: return as-is, their contents are data not code
      ((memq (car expr) '(quote function))
       expr)
      ((memq (car expr) '(ldef defun defmacro))
       ;; For function definitions, transform the body but not the signature
       (if (>= (length expr) 4)
           `(,(car expr) ,(cadr expr) ,(caddr expr)
             ,@(mapcar #'l--transform-curry-calls (cdddr expr)))
         expr))
      ((memq (car expr) '(let let*))
       ;; For let forms, transform bindings and body
       `(,(car expr)
         ,(mapcar (lambda (binding)
                    (if (consp binding)
                        `(,(car binding) ,(l--transform-curry-calls (cadr binding)))
                      binding))
                  (cadr expr))
         ,@(mapcar #'l--transform-curry-calls (cddr expr))))
      (t
       ;; For other special forms, transform arguments recursively
       `(,(car expr) ,@(mapcar #'l--transform-curry-calls (cdr expr))))))
   ((and (consp expr)
         (consp (car expr))
         (not (eq (caar expr) 'lambda)))
    ;; Transform ((fn args) more-args) to (funcall (fn args) more-args)
    `(funcall ,(l--transform-curry-calls (car expr)) ,@(mapcar #'l--transform-curry-calls (cdr expr))))
   ((consp expr)
    (mapcar #'l--transform-curry-calls expr))
   (t expr)))

(defun l--group-doc-expressions (body)
  "Group @doc expressions with their following expression.
Transforms: @doc \"...\" (ldef ...) -> (@doc \"...\" (ldef ...))
BODY is a list of expressions.

since: 0.3.0"
  (let ((result '())
        (remaining body))
    
    (while remaining
      (let ((current (car remaining)))
        (if (eq current '@doc)
            ;; Found @doc - group with next two expressions (docstring + ldef)
            (if (>= (length remaining) 3)
                (progn
                  (push `(@doc ,(cadr remaining) ,(caddr remaining)) result)
                  (setq remaining (cdddr remaining)))
              ;; Not enough expressions after @doc - treat as regular expression
              (progn
                (push current result)
                (setq remaining (cdr remaining))))
          ;; Regular expression
          (progn
            (push current result)
            (setq remaining (cdr remaining))))))
    
    (nreverse result)))

(provide 'l-main)
;;; l-main.el ends here
