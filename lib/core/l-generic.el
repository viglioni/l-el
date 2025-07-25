;;; l-generic.el --- Generic function dispatch with pattern matching -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laura Viglioni

;; Author: Laura Viglioni
;; Keywords: lisp, functional, programming, generics, pattern-matching
;; URL: https://github.com/viglioni/l-el

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

;; This module provides generic function dispatch with pattern matching
;; capabilities for the l.el library.  It enables the creation of functions
;; that can have multiple implementations based on argument patterns,
;; including type matching, value matching, and wildcard patterns.

;;; Code:

(require 'cl-lib)
(require 'l-generic-type-predicates)
(require 'l-generic-state)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; l-generic dispatcher ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defun l-generic--calculate-specificity (pattern-list)
  "Calculate specificity score for PATTERN-LIST.

Returns a numeric score indicating how specific the pattern is.
Higher scores indicate more specific patterns that should be
matched before more general ones.

Scoring rules:
- Value match: 1000 points per pattern
- Type match: 100 points per pattern
- Wildcard with binding: 1 point per pattern
- Regular parameter: 1 point per pattern

PATTERN-LIST is a list of patterns, where each pattern can be:
- A symbol: regular parameter or wildcard
- A list: \(param type-or-value) for type/value matching

Examples:
  \(l-generic--calculate-specificity \\='(x y))
  ;; => 2 (two regular parameters)
  
  \(l-generic--calculate-specificity \\='((x :integer) (y :string)))
  ;; => 200 (two type matches)
  
  \(l-generic--calculate-specificity \\='((x \"hello\") y))
  ;; => 1001 (one value match + one regular parameter)
  
  \(l-generic--calculate-specificity \\='(_ignore x))
  ;; => 2 (wildcard + regular parameter)"
  (cl-reduce
   #'+
   (mapcar (lambda (pattern)
             (cond ((listp pattern)
                    (let ((spec (cadr pattern)))
                      (cond ((keywordp spec) 100) ; type match
                            (t 1000))))           ; value match
                   ((and (symbolp pattern) ; wildcard with binding
                         (string-prefix-p "_" (symbol-name pattern))) 1)
                   (t 1)))              ; regular wildcard
           pattern-list)
   :initial-value 0))

(defun l-generic--generate-pattern-condition (pattern arg-index)
  "Generate condition for matching PATTERN against argument at ARG-INDEX.

Returns a condition expression that will be used in the generated
dispatch function to test whether the argument at ARG-INDEX matches
the given PATTERN.

PATTERN can be:
- A symbol: always matches (returns t)
- A symbol starting with \"_\": wildcard, always matches (returns t)
- A list \(param `:type'): generates type predicate call
- A list \(param value): generates equality check

ARG-INDEX is the zero-based index of the argument to test.

Examples:
  \(l-generic--generate-pattern-condition \\='x 0)
  ;; => t (always matches)
  
  \(l-generic--generate-pattern-condition \\='(x :integer) 0)
  ;; => (integerp (nth 0 args))
  
  \(l-generic--generate-pattern-condition \\='(x \"hello\") 1)
  ;; => (equal (nth 1 args) \"hello\")
  
  \(l-generic--generate-pattern-condition \\='_ignore 2)
  ;; => t (wildcard always matches)"
  (cond
   ((listp pattern)
    (let ((spec (cadr pattern)))
      (cond
       ((eq spec :rest)
        ;; Rest parameters always match (handled specially)
        t)
       ((keywordp spec)
        ;; Type match: (arg :integer) -> (integerp (nth 0 args))
        (let ((predicate (cdr (assoc spec l-generic-type-predicates))))
          (if predicate
              `(,predicate (nth ,arg-index args))
            (error "Unknown type predicate: %s" spec))))
       (t
        ;; Value match: (arg "value") -> (equal (nth 0 args) "value")
        `(equal (nth ,arg-index args) ,spec)))))
   ((and (symbolp pattern)
         (string-prefix-p "_" (symbol-name pattern)))
    ;; Wildcard with binding: always true
    t)
   (t
    ;; Regular parameter: always true
    t)))

(defun l-generic--generate-bindings (pattern-list)
  "Generate let bindings for PATTERN-LIST parameters.

Returns a list of binding forms suitable for use in a let expression.
Each binding associates a parameter name with the corresponding
argument value from the args list.

PATTERN-LIST is a list of patterns, where each pattern can be:
- A symbol: bound to the corresponding argument
- A list (param spec): param is bound to the corresponding argument

Examples:
  (l-generic--generate-bindings \\='(x y z))
  ;; => ((x (nth 0 args)) (y (nth 1 args)) (z (nth 2 args)))
  
  (l-generic--generate-bindings \\='((x :integer) (y \"hello\") z))
  ;; => ((x (nth 0 args)) (y (nth 1 args)) (z (nth 2 args)))
  
  (l-generic--generate-bindings \\='(_ignore x))
  ;; => ((_ignore (nth 0 args)) (x (nth 1 args)))"
  (let ((rest-pos (cl-position-if (lambda (pattern)
                                   (and (listp pattern)
                                        (eq (cadr pattern) :rest)))
                                 pattern-list)))
    (if rest-pos
        ;; Handle rest parameter
        (append
         ;; Fixed parameters
         (cl-loop for pattern in (cl-subseq pattern-list 0 rest-pos)
                  for i from 0
                  collect (let ((param (if (listp pattern) (car pattern) pattern)))
                            `(,param (nth ,i args))))
         ;; Rest parameter
         (list (let ((rest-pattern (nth rest-pos pattern-list)))
                 `(,(car rest-pattern) (nthcdr ,rest-pos args)))))
      ;; No rest parameter - normal binding
      (cl-loop for pattern in pattern-list
               for i from 0
               collect (let ((param (if (listp pattern) (car pattern) pattern)))
                         `(,param (nth ,i args)))))))

(cl-defmethod l-generic--generate-method-clause ((method l-generic-method-spec))
  "Generate a cond clause for METHOD.

Returns a cond clause that tests the method's pattern conditions
and executes the method body if all conditions match.

METHOD is a struct `l-generic-method-spec' containing:
- `arity': number of arguments (not used in generated code)
- `body': list of expressions to execute
- `pattern-list': list of patterns for matching
- `specificity': numeric score (not used in generated code)

The generated clause has the form:
  \((and condition1 condition2 ...)
   (let ((param1 (nth 0 args)) (param2 (nth 1 args)) ...)
     body...))

Conditions that always return t are removed from the and expression
for optimization.

Examples:
  \(l-generic--generate-method-clause
    \\='(1100 2 ((x :integer) (y :string)) (+ x (length y))))
  ;; => ((and (integerp (nth 0 args)) (stringp (nth 1 args)))
  ;;     (let ((x (nth 0 args)) (y (nth 1 args)))
  ;;       (+ x (length y))))
  
  (l-generic--generate-method-clause
    \\='(2 2 (x y) (list x y)))
  ;; => (t (let ((x (nth 0 args)) (y (nth 1 args)))
  ;;         (list x y)))"
  (let* ((pattern-list (l--pattern-list method)) ;;method[]
         (body         (l--body method)) ;; func-body
         (bindings     (l-generic--generate-bindings pattern-list)) ;; list
         (conditions   (cl-loop for pattern in pattern-list
                                for i from 0
                                collect (l-generic--generate-pattern-condition
                                         pattern i)))) ;; list of cond patterns
    
    `((and ,@(remove t conditions))  ; Remove 'always true' conditions
      (let ,bindings
        ,@body))))

(defun l-generic--generate-dispatch-function (name methods)
  "Generate the complete dispatch function for NAME with METHODS.

Returns a defun form that implements the complete dispatch logic
for the generic function.  The generated function:

1. Checks the number of arguments (arity)
2. For each valid arity, tests method patterns in specificity order
3. Executes the first matching method
4. Falls back to currying if no exact arity match is found
5. Raises an error if no pattern matches for the given arity

NAME is the function name symbol.
METHODS is a list of `l-generic-method-spec' structures, each containing:
- `arity': number of arguments for dispatch optimization
- `body': code to execute when matched
- `pattern-list': the actual patterns to match
- `specificity': numeric score for pattern matching priority

The generated function handles:
- Pattern matching with type and value constraints
- Automatic currying for partial application
- Proper error messages for unmatched patterns

Examples:
  Given methods for a function calc:
  - method with specificity 1100, arity 2, pattern ((op \='+) x y), body (+ x y)
  - method with specificity 1100, arity 2, pattern ((op \='*) x y), body (* x y)
  - method with specificity 1, arity 2, pattern (_ _ _), body (error \"Unknown operation\")
  
  The generated function would:
  - Match (calc \='+ 2 3) to first method, return 5
  - Match (calc \='* 2 3) to second method, return 6
  - Match (calc \='unknown 2 3) to third method, raise error
  - Partially apply (calc \='+) to return a curried function"
  (let* ((methods-by-arity (cl-loop for method in methods
                                    for arity = (l--arity method)
                                    collect (cons arity method))) ;; (arity . methods)
         (max-arity (if methods (apply #'max (mapcar #'car methods-by-arity)) 0)) ;; int
         (min-arity (if methods (apply #'min (mapcar #'car methods-by-arity)) 0)) ;; int
         (rest-methods (l-generic--rest-methods methods)) ;; method[]
         (min-rest-arity (if rest-methods
                            (apply #'min (mapcar 'l--arity rest-methods))
                          most-positive-fixnum))
         (arity-groups (cl-loop for arity from min-arity to max-arity
                               collect (cons arity
                                           (cl-remove-if-not
                                            (lambda (method) (= (l--arity method) arity))
                                            methods)))))
    
    `(defun ,name (&rest args)
       ,(l-generic--doc name)
       (let ((arity (length args)))

         ;; Writing all methods inside defun, following the order:
         ;;(cond
         ;; Branch 1: Fixed-arity methods
         ;; Branch 2: Rest methods
         ;; Branch 3: Currying
         ;; Branch 4: Error
         ;; )
         (cond
          ;; fixed arity methods
          ,@(cl-loop for (arity . arity-methods) in arity-groups
                     when arity-methods
                     collect `((= arity ,arity)
                               (cond
                                ,@(mapcar #'l-generic--generate-method-clause arity-methods)
                                (t (error "PatternMatch error in '%s': couldn't match %S"
                                          ',name args)))))
          ;; Handle rest methods for args >= min-rest-arity
          ,@(when rest-methods
              `(((>= arity ,min-rest-arity)
                 (cond
                  ,@(mapcar #'l-generic--generate-method-clause rest-methods)
                  (t (error "PatternMatch error in '%s': couldn't match %S"
                            ',name args))))))
          ;; Currying case - only for insufficient args
          ((< arity ,min-arity) (apply #'lpartial #',name args))
          ;; Too many args - error
          (t (error "PatternMatch error in '%s': no method for %d arguments.  Available: %S"
                    ',name arity ',(mapcar #'car methods-by-arity))))))))

(cl-defmethod l-generic--doc ((fname symbol))
  "Build documentation for FNAME."
  (format "%s is a generic function.
Check `ldef' for more documentation.

General documentation: %s"
          fname
          (l--get-doc-registry fname)))



(defun l-generic--rest-methods (methods)
  "Return methods from METHODS that have a `:rest' param.
METHODS are a list of `l-generic-method-spec'."
  (cl-remove-if-not ;; list of methods that contains :rest in the params
   (lambda (method)
     (cl-some (lambda (pattern) (and (listp pattern) ;; Short-circuit: remove symbol params
                                (eq (cadr pattern) :rest)))
              (l--pattern-list method)))
   methods))

(defun l-generic--add-method (name arity pattern-list body)
  "Add a method to the registry and regenerate dispatch function.

This function registers a new method implementation for a generic function
and automatically regenerates the dispatch function to include the new method.
Methods are ordered by specificity to ensure the most specific patterns
are matched before more general ones.

The function performs the following steps:
1. Calculate specificity score for the pattern-list
2. Create a method specification with all necessary metadata
3. Retrieve existing methods for the function from the registry
4. Add the new method and sort all methods by specificity (descending)
5. Update the registry with the sorted method list
6. Regenerate and evaluate the complete dispatch function

NAME is the symbol representing the generic function name.
ARITY is the number of arguments the method accepts.
PATTERN-LIST is a list of patterns for argument matching, where each
pattern can be:
- A symbol: regular parameter (matches any value)
- A symbol starting with \"_\": wildcard parameter
- A list (param :type): type constraint matching
- A list (param value): exact value matching

BODY is a list of expressions that form the method implementation.

The specificity scoring ensures proper method dispatch order:
- Value matches: 1000 points each
- Type matches: 100 points each
- Wildcards and regular parameters: 1 point each

Examples:
  (l-generic--add-method \\='my-func 2 \\='((x :integer) (y :string))
                         \\='((+ x (length y))))
  ;; Adds a method that matches integer + string arguments
  
  (l-generic--add-method \\='my-func 2 \\='((x \"hello\") y)
                         \\='((concat x (symbol-name y))))
  ;; Adds a method that matches \"hello\" + any second argument

After calling this function, the generic function NAME will be updated
to include the new method and will dispatch to it when the pattern matches.
The dispatch function is immediately regenerated and evaluated, making
the new method available for use.

This is an internal function used by the `l-generic' macro and should
not be called directly by user code."
  
  (l-generic--check-rest-syntax! name pattern-list)
  
  (let* ((specificity     (l-generic--calculate-specificity pattern-list)) ;; int
         (method          (l--method arity body pattern-list specificity)) ;; method
         (current-methods (l--get-from-registry name)) ;; method[]
         (sorted-methods  (l-generic--add-and-sort method current-methods))) ;; method[]
    
    ;; new method and sort by specificity (descending)
    (l--add-to-registry name sorted-methods)
    
    ;; Regenerate dispatch function
    (eval (l-generic--generate-dispatch-function name sorted-methods))))

(cl-defmethod l-generic--add-and-sort ((method l-generic-method-spec) (methods list))
  "Create a sorted list by specificity using METHOD and METHODS.
Check `l-generic-method-spec'."
  (sort (cons method methods)
        (lambda (a b) (> (l--specificity a) (l--specificity b)))))


(defun l-generic--check-rest-syntax! (name pattern-list)
  "Check PATTERN-LIST to see if the `:rest' predicate is used correctly.
NAME is the name of the method.

Examples:
\(ldef foo (a (b :number) (c :rest))...) ;; correct
\(ldef foo (a (b :rest) (c :rest))...)   ;; incorrect
\(ldef foo (a (b :rest) (c :string))...) ;; incorrect"
  (let ((rest-positions (cl-loop for pattern in pattern-list
                                 for i from 0
                                 when (and (listp pattern) (eq (cadr pattern) :rest))
                                 collect i)))

    ;; rest position validations
    (when rest-positions
      (when (> (length rest-positions) 1)
        (error "Only one :rest parameter allowed in function '%s'" name))
      (when (/= (car rest-positions) (1- (length pattern-list)))
        (error ":rest parameter must be the last parameter in function '%s'" name)))))

(defun l-generic-cleanup (name)
  "Remove generic function NAME and all its methods.

This function completely removes a generic function from the l-generic
registry and unbinds the function symbol, effectively deleting the
function and all its associated method definitions.

This is useful for:
- Cleaning up during development when redefining generic functions
- Removing functions that are no longer needed
- Resolving conflicts when function names are reused
- Debugging generic function dispatch issues

NAME is the symbol representing the generic function to remove.
When called interactively, prompts for the function name.

After calling this function:
- The function will no longer be callable
- All method definitions for the function are lost
- The function name becomes available for redefinition
- No currying or pattern matching behavior remains

Examples:
  ;; Define a generic function
  \(ldef my-func (x) (+ x 1))
  
  ;; Remove the function completely
  \(l-generic-cleanup \='my-func)
  
  ;; Function is no longer available
  \(my-func 5)        ;; ERROR: void-function my-func

Note: This operation cannot be undone.  All method definitions
for the function are permanently removed from the registry.
The function must be redefined from scratch if needed again.

Interactive usage:
   \\[l-generic-cleanup] RET my-func RET

See also: `ldef' for defining generic functions."
  (interactive "SGeneric function name: ")
  (remhash name l-generic-method-registry)
  (fmakunbound name))

(defmacro l-generic (name args &rest body)
  "Define a method for generic function NAME with pattern matching and currying.

This macro is the underlying implementation for `ldef' and should not be used
directly.  Use `ldef' instead for defining generic functions with pattern
matching and automatic currying capabilities.

Creates a function NAME that supports:
- Pattern matching on arguments (type, value, and wildcard patterns)
- Automatic currying when called with fewer arguments
- Multiple method definitions with different patterns
- &rest argument handling

ARGS can be:
- A list of regular parameters for fixed-arity functions
- A list including &rest for variadic functions
- Patterns for matching (see `ldef' documentation)

For &rest arguments, creates a wrapper function that:
- Binds fixed arguments to their parameter names
- Binds remaining arguments to the &rest parameter
- Falls back to partial application if insufficient arguments

For fixed-arity functions, registers the method in the generic function
registry and regenerates the dispatch function.

Methods are ordered by specificity (most specific patterns first):
1. Value matches (1000 points each)
2. Type matches (100 points each)
3. Wildcards and regular parameters (1 point each)

NAME is the function name to define.
ARGS is the parameter list, potentially including patterns and &rest.
BODY is the function body to execute when pattern matches."
      
      
      ;; Check for &rest and error
      (when (cl-position '&rest args)
        (error "Use (param :rest) instead of &rest for pattern matching functions"))
      
      ;; Regular fixed-arity function
      `(progn
         (l-generic--add-method ',name ,(length args) ',args ',body)
         ',name))

(cl-defmethod l-generic-doc ((fname symbol) (docstring string))
  "Add DOCSTRING to FNAME defined with `ldef'."
  (l--add-doc-registry fname docstring)
;;  (l-generic--generate-dispatch-function)
  (print fname)
  ;;(print (l--get-from-registry fname))
  )

(provide 'l-generic)
;;; l-generic.el ends here
