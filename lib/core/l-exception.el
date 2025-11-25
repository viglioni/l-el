;;; l-exception.el --- Exception handling utilities for l.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laura Viglioni

;; Author: Laura Viglioni
;; Keywords: lisp, exceptions, errors
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

;; This module provides standardized exception handling utilities for l.el.
;; It defines functions for raising semantic errors with consistent formatting.

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define error conditions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define l-error as the parent error condition
(define-error 'l-error "l.el error")

;; Define specific error types as children of l-error
(define-error 'l-pattern-match-error "Pattern match error" 'l-error)
(define-error 'l-type-mismatch-error "Type mismatch in pattern matching" 'l-error)
(define-error 'l-arity-error "Wrong number of arguments" 'l-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error raising functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun l--raise-type-mismatch (predicate value &optional context)
  "Raise a type mismatch error.
PREDICATE is the expected type predicate (e.g., \\='functionp).
VALUE is the actual value that failed the predicate.
CONTEXT is an optional context string."
  (signal 'l-type-mismatch-error
          (if context
              (list predicate value context)
            (list predicate value))))

(defun l--raise-arity-error (function-name expected actual)
  "Raise an arity error.
FUNCTION-NAME is the name of the function.
EXPECTED is the expected number of arguments or list of valid arities.
ACTUAL is the actual number of arguments received."
  (signal 'l-arity-error
          (list function-name expected actual)))

(defun l--raise-pattern-match-error (function-name args)
  "Raise a pattern match error.
FUNCTION-NAME is the name of the function.
ARGS is the list of arguments that couldn't be matched."
  (signal 'l-pattern-match-error
          (list function-name args)))

(defun l--raise-error (format-string &rest args)
  "Raise a generic l-error with FORMAT-STRING and ARGS."
  (signal 'l-error (list (apply #'format format-string args))))

(cl-defun l-raise (error-type &key predicate value context function-name expected actual args format-string)
  "Raise an error of ERROR-TYPE with appropriate arguments.

ERROR-TYPE determines the kind of error to signal.
Supported error types:
  \\='type-mismatch - Type mismatch in pattern matching
  \\='arity-error - Wrong number of arguments
  \\='pattern-match - Pattern match failure
  \\='error - Generic l-error

For \\='type-mismatch:
  :predicate - Expected type predicate (e.g., \\='functionp)
  :value - Actual value that failed the predicate
  :context - Optional context string
  Example: (l-raise \\='type-mismatch :predicate \\='functionp :value 42 :context \"in function foo\")

For \\='arity-error:
  :function-name - Name of the function
  :expected - Expected number of arguments or list of valid arities
  :actual - Actual number of arguments received
  Example: (l-raise \\='arity-error :function-name \\='my-func :expected 2 :actual 3)

For \\='pattern-match:
  :function-name - Name of the function
  :args - List of arguments that couldn't be matched
  Example: (l-raise \\='pattern-match :function-name \\='my-func :args (list 1 2))

For \\='error:
  :format-string - Format string for the error message
  :args - List of arguments for the format string
  Example: (l-raise \\='error :format-string \"Unknown value: %s\" :args (list val))"
  (pcase error-type
    ('type-mismatch
     (l--raise-type-mismatch predicate value context))

    ('arity-error
     (l--raise-arity-error function-name expected actual))

    ('pattern-match
     (l--raise-pattern-match-error function-name args))

    ('error
     (apply #'l--raise-error format-string args))

    (_ (error "Unknown error type: %s" error-type))))

(provide 'l-exception)
;;; l-exception.el ends here
