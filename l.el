;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; l.el --- Modern functional programming utilities for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laura Viglioni

;; Author: Laura Viglioni
;; Version: 0.1.0
;; Package-Requires: ((emacs "29"))
;; Keywords: lisp, functional, programming, utilities
;; URL: https://github.com/lauravglioni/l

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

;; Laurisp provides a modern functional programming approach to writing
;; Emacs Lisp, drawing inspiration from Common Lisp, Haskell and Elixir.
;;
;; This library introduces currying, partial application, pattern matching,
;; and placeholder substitution utilities that make Emacs Lisp more expressive
;; and closer to modern functional programming languages.
;;
;; Key features:
;; - Automatic currying with `ldef'
;; - Pattern matching with `ldef'
;; - Type matching with `ldef'
;; - Partial application with `l-partial'
;; - Placeholder substitution with `__'
;; - Custom syntax `with-l'
;; - Optional syntax transformation via `l-syntax'
;;
;; Configuration:
;; The `l-syntax' variable controls syntax transformation behavior.
;; It can be set globally:
;;
;;   (setq l-syntax t)
;;
;; Or locally in a file using a property line:
;;
;;   ;; -*- l-syntax: t; -*-
;;
;; When enabled, this allows for more concise syntax transformations
;; and enhanced readability in functional compositions.
;;
;; Example usage:
;;
;;   (ldef add3 (x y z) (+ x y z))
;;   (funcall (add3 1 2) 3) ; => 6
;;
;;   (ldef greet ((name "Alice")) "Hello, Alice!")
;;   (ldef greet (name) (concat "Hi, " name "!"))
;;   (greet "Alice") ; => "Hello, Alice!"
;;   (greet "Bob")   ; => "Hi, Bob!"
;;
;;   (with-l
;;     ((add3 1) 2 3)) ; => 6
;;
;;   (__ (+ __ (* __ 2)) 5) ; => 15
;;
;;   (funcall (l-partial '+ 10) 5) ; => 15

;;; Code:

;; Adding ./lib to load-path
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name "lib" (file-name-directory (or load-file-name buffer-file-name))))


(require 'cl-lib)
(require 'l-generic)

;;;;;;;;;
;; API ;;
;;;;;;;;;

(defvar l-syntax nil
  "Controls whether l syntax transformations are applied during evaluation.

When set to t globally, l syntax transformations will be applied
to all elisp evaluation operations without requiring file-local variable
declarations in individual files.

When set to nil (the default), l syntax transformations will only
be applied to files that explicitly declare l-syntax as a file-local
variable using either:

  ;; -*- l-syntax: t; -*-

or in the local variables section at the end of the file:

  ;; Local Variables:
  ;; l-syntax: t
  ;; End:

This variable affects the behavior of `eval-last-sexp', `eval-region',
`eval-buffer', `load-file', and `load' when the appropriate advice
functions are installed via `l-syntax-advices'.

Setting this to t globally allows you to use l syntax everywhere
without adding file-local variable declarations to each file, while
setting it to nil provides more granular control on a per-file basis.")

(defun l-syntax-advices ()
  "Add advice to evaluation functions for l syntax support.
This function adds around advice to `eval-last-sexp', `eval-region',
`eval-buffer', `load-file', and `load' to enable l syntax processing."
  (interactive)
  (advice-add 'eval-last-sexp :around #'l--eval-last-sexp-advice)
  (advice-add 'eval-region    :around #'l--eval-region-advice)
  (advice-add 'eval-buffer    :around #'l--eval-buffer-advice)
  (advice-add 'load-file      :around #'l--load-file-advice)
  (advice-add 'load           :around #'l--load-file-advice))

(defun l-syntax-remove-advices ()
  "Remove advice to evaluation functions for l syntax support.
This function adds around advice to `eval-last-sexp', `eval-region',
`eval-buffer', `load-file', and `load' to enable l syntax processing."
  (interactive)
  (advice-remove 'eval-last-sexp #'l--eval-last-sexp-advice)
  (advice-remove 'eval-region    #'l--eval-region-advice)
  (advice-remove 'eval-buffer    #'l--eval-buffer-advice)
  (advice-remove 'load-file      #'l--load-file-advice)
  (advice-remove 'load           #'l--load-file-advice))

(defun l-partial (fn &rest init-args)
  "Return a partially applied function with FN and INIT-ARGS.

Creates a new function that, when called, applies FN to the
combination of INIT-ARGS (provided now) and any additional
arguments (provided later).

Examples:
  (funcall (l-partial \\='+ 5) 3)     ;; => 8
  (funcall (l-partial \\='* 2 3) 4)   ;; => 24
  (funcall (l-partial \\='concat \"Hello, \") \"World!\")
  ;; => \"Hello, World!\"

FN can be a function symbol, lambda expression, or any callable.
INIT-ARGS are the initial arguments to partially apply to FN."
  (lambda (&rest args)
    (apply fn (append init-args args))))

(defmacro ldef (name args &rest body)
  "Define autocurried functions with pattern matching support.

Creates a function NAME that automatically curries when called with fewer
arguments and supports pattern matching on arguments.

ARGS is a list of parameter patterns supporting:
- Regular parameters: arg
- Wildcards: _ignore, _var (bind but conventionally ignore)
- Type matches: (arg :integer), (arg :string), etc.
- Value matches: (arg \"specific-value\"), (arg 42), etc.

Methods are ordered by specificity (most specific first):
1. Value matches (1000 points each)
2. Type matches (100 points each)
3. Wildcards (1 point each)

PATTERN MATCHING:
Arguments can be specified as either symbols or lists for pattern matching.
- Symbol: x - matches any value, binds to x
- Wildcard: _ignore - matches any value, binds but conventionally ignored
- Type match: (x :integer) - matches only when x satisfies integerp
- Value match: (x \"value\") - matches only when x equals \"value\"

Pattern matching examples:
  (ldef fib ((n 0)) 0)                    ;; matches when n = 0
  (ldef fib ((n 1)) 1)                    ;; matches when n = 1
  (ldef fib (n) (+ (fib (- n 1)) (fib (- n 2))))  ;; general case

  (ldef greet ((name \"Alice\")) \"Hello, Alice!\")  ;; matches \"Alice\"
  (ldef greet (name) (concat \"Hi, \" name \"!\"))   ;; general case

  (ldef calc ((op '+) x y) (+ x y))       ;; matches when op = '+
  (ldef calc ((op '*) x y) (* x y))       ;; matches when op = '*
  (ldef calc (_op _x _y) (error \"Unknown operation\"))  ;; fallback

CURRYING:
Functions defined with ldef automatically curry when called
with fewer arguments.

Currying examples:
  (ldef add3 (x y z) (+ x y z))
  (add3 1 2 3)        ;; => 6 (full application)
  (funcall (add3 1) 2 3)  ;; => 6 (partial application)
  (funcall (funcall (add3 1) 2) 3)  ;; => 6 (chained partial)

NAME is the function name to define.
ARGS is a list of parameter patterns.
BODY is the function body to execute when pattern matches and fully applied."
  `(l-generic ,name ,args ,@body))

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
Regular function calls and other expressions are left unchanged."
  `(progn ,@(mapcar #'l--transform-curry-calls body)))

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
the macro will not work correctly."
  (let* ((pos (cl-position '-> expr))
         (args (cl-subseq expr 0 pos))
         (body (cl-subseq expr (1+ pos)))
         )
    `(lambda ,args ,@body)))

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
expressions are handled correctly."
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
- Regular expressions are recursively processed
- Atoms are left unchanged

EXPR is the expression to transform, can be an atom, list, or nested structure.
Returns the transformed expression with curried calls
converted to funcall forms."
  (cond
   ((and (consp expr)
         (consp (car expr))
         (not (eq (caar expr) 'lambda)))
    ;; Transform ((fn args) more-args) to (funcall (fn args) more-args)
    `(funcall ,(l--transform-curry-calls (car expr)) ,@(cdr expr)))
   ((consp expr)
    (mapcar #'l--transform-curry-calls expr))
   (t expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use l syntax without `with-l' ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun l--process-file-content (content)
  "Transform file CONTENT through with-l."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((forms '()))
      (while (not (eobp))
        (condition-case nil
            (push (read (current-buffer)) forms)
          (end-of-file nil)))
      (eval `(with-l ,@(nreverse forms))))))


(defun l--load-file-advice (orig-fun file &optional noerror nomessage)
  "Advice for load-file to handle l-syntax."
  (if (and (stringp file) (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((l-syntax (hack-local-variables-prop-line)))
          (if (cdr (assq 'l-syntax l-syntax))
              ;; Transform through with-l
              (l--process-file-content (buffer-string))
            ;; Regular loading
            (funcall orig-fun file noerror nomessage))))
    (funcall orig-fun file noerror nomessage)))


(defun l--load-advice (orig-fun file &optional noerror nomessage nosuffix must-suffix)
  "Advice for load to handle l-syntax."
  (if (and (stringp file) (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((l-syntax (hack-local-variables-prop-line)))
          (if (cdr (assq 'l-syntax l-syntax))
              ;; Transform through with-l
              (l--process-file-content (buffer-string))
            ;; Regular loading
            (funcall orig-fun file noerror nomessage nosuffix must-suffix))))
    (funcall orig-fun file noerror nomessage nosuffix must-suffix)))


(defun l--check-file-local-vars ()
  "Check if current buffer has l-syntax enabled.

This function examines the current buffer for file-local variables,
specifically looking for the `l-syntax' variable.  It processes
both the prop-line (first line) and the file-local variables section
at the end of the file.

This function is typically used to determine whether l syntax
processing should be applied to the current buffer."
  (hack-local-variables-prop-line)
  (hack-local-variables))


(defun l--eval-last-sexp-advice (orig-fun &optional eval-last-sexp-arg-internal)
  "Advice for `eval-last-sexp' to handle l-syntax.

This around advice function intercepts calls to `eval-last-sexp' and
checks if the current buffer has `l-syntax' enabled as a buffer-local
variable.  If so, it wraps the preceding S-expression in a `with-l'
form before evaluation.

ORIG-FUN is the original `eval-last-sexp' function.
EVAL-LAST-SEXP-ARG-INTERNAL is the optional argument
passed to the original function.

The wrapping is achieved by temporarily redefining `elisp--preceding-sexp'
to return the S-expression wrapped in `with-l', allowing the original
function to handle all other aspects of evaluation including output formatting."
  (if (and (boundp 'l-syntax) l-syntax)
      (let ((sexp (elisp--preceding-sexp)))
        ;; Wrap in with-l and let original function handle everything
        (cl-letf (((symbol-function 'elisp--preceding-sexp)
                   (lambda () `(with-l ,sexp))))
          (funcall orig-fun eval-last-sexp-arg-internal)))
    (funcall orig-fun eval-last-sexp-arg-internal)))

(defun l--eval-region-advice (orig-fun start end &optional printflag read-function)
  "Advice for `eval-region' to handle l-syntax.

This around advice function intercepts calls to `eval-region' and
checks if the current buffer has `l-syntax' enabled as a
buffer-local variable.
If so, it wraps the region content in a `with-l' form before evaluation.

ORIG-FUN is the original `eval-region' function.
START and END define the region boundaries.
PRINTFLAG and READ-FUNCTION are optional arguments
passed to the original function.

The wrapping is achieved by temporarily redefining
`buffer-substring-no-properties' to return the region content wrapped in
`with-l', allowing the original function
to handle all other aspects of evaluation."
  (if (and (boundp 'l-syntax) l-syntax)
      (let ((original-code (buffer-substring-no-properties start end)))
        ;; Wrap in with-l and let original function handle everything
        (cl-letf (((symbol-function 'buffer-substring-no-properties)
                   (lambda () (format "(with-l %s)" original-code))))
          (funcall orig-fun start end printflag read-function)))
    (funcall orig-fun start end printflag read-function)))

(defun l--eval-buffer-advice (orig-fun &optional buffer printflag filename unibyte)
  "Advice for `eval-buffer' to handle l-syntax.

This around advice function intercepts calls to `eval-buffer' and
checks if the target buffer has `l-syntax' enabled as a buffer-local
variable.  If so, it wraps the entire buffer content in a `with-l'
form before evaluation.

ORIG-FUN is the original `eval-buffer' function.
BUFFER is the buffer to evaluate (defaults to current buffer).
PRINTFLAG, FILENAME, and UNIBYTE are optional arguments passed to
the original function.

The wrapping is achieved by temporarily redefining `buffer-string'
to return the buffer content wrapped in `with-l', allowing the original
function to handle all other aspects of evaluation."
  (with-current-buffer (or buffer (current-buffer))
    (if (and (boundp 'l-syntax) l-syntax)
        (let ((original-content (buffer-string)))
          ;; Wrap in with-l and let original function handle everything
          (cl-letf (((symbol-function 'buffer-string)
                     (lambda () (format "(with-l %s)" original-content))))
            (funcall orig-fun buffer printflag filename unibyte)))
      (funcall orig-fun buffer printflag filename unibyte))))

(provide 'l)
;;; l.el ends here
