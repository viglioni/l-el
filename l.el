;;; package --- Summary ;;; -*- lexical-binding: t; -*-
;;; l.el --- Modern functional programming utilities for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laura Viglioni

;; Author: Laura Viglioni
;; Version: 0.2.0
;; Package-Requires: ((emacs "29"))
;; Keywords: lisp, functional, programming, utilities
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

;; l.el provides a modern functional programming approach to writing
;; Emacs Lisp, drawing inspiration from Common Lisp, Haskell and Elixir.
;;
;; This comprehensive library introduces currying, partial application,
;; pattern matching, placeholder substitution, syntax transformations,
;; and specialized development tools that make Emacs Lisp more expressive
;; and closer to modern functional programming languages.
;;
;; Key features:
;; - Automatic currying with `ldef'
;; - Advanced pattern matching with `ldef' (value, type, :rest parameters)
;; - Type matching with comprehensive predicate system
;; - Partial application with `lpartial'
;; - Placeholder substitution with `__'
;; - Lambda syntax with `l' and arrow notation
;; - Custom syntax transformation with `with-l'
;; - File-level syntax transformation via `l-syntax'
;; - Documentation annotations with `@doc'
;; - Enhanced major mode with `l-mode'
;; - Specialized loading with `l-require'
;; - Function composition utilities
;; - Generic function cleanup and management
;;
;; Syntax Transformation:
;; The `l-syntax' system provides automatic syntax transformation for
;; evaluation operations.  It can be configured in multiple ways:
;;
;; Global configuration:
;;   (setq l-syntax t)
;;   (l-syntax-advices)  ; Enable automatic transformation
;;
;; File-local configuration using property line:
;;   ;; -*- l-syntax: t; -*-
;;
;; File-local configuration using local variables:
;;   ;; Local Variables:
;;   ;; l-syntax: t
;;   ;; End:
;;
;; When enabled, l-syntax automatically wraps expressions in `with-l'
;; during `eval-last-sexp', `eval-region', `eval-buffer', `load-file',
;; and `load' operations, enabling natural curried function syntax
;; without explicit `with-l' wrapping.
;;
;; Enhanced Major Mode:
;; l-mode extends emacs-lisp-mode with specialized syntax highlighting:
;; - Enhanced highlighting for @doc annotations
;; - Automatic activation when l-syntax is enabled
;; - Full Emacs Lisp compatibility
;;
;; Documentation System:
;; The @doc macro provides enhanced documentation capabilities:
;;   (@doc "Adds two numbers together."
;;    (ldef add (x y) (+ x y)))
;;
;; When l-syntax is t, you can use Elixir-style syntax without parentheses:
;;   @doc "Adds two numbers together."
;;   (ldef add (x y) (+ x y))
;;
;; This associates documentation with generic functions defined using ldef.
;;
;; Advanced Pattern Matching:
;; ldef supports sophisticated pattern matching including:
;; - Value matching: (arg "specific-value")
;; - Type matching: (arg :integer), (arg :string), etc.
;; - Rest parameters: (args :rest) for variadic functions
;; - Wildcard patterns: _ignore, _var
;; - Automatic specificity ordering for method dispatch
;;
;; Type System:
;; Comprehensive type predicate system supporting:
;; :buffer, :callable, :cons, :float, :function, :hash-table,
;; :integer, :list, :null, :number, :sequence, :string, :symbol, :vector
;;
;; Specialized Loading:
;; l-require provides enhanced library loading with automatic l-syntax
;; processing for files that declare l-syntax support.
;;
;; Function Utilities:
;; Additional utilities for functional programming including:
;; - Function composition with `lcomp'
;; - Enhanced currying and partial application
;; - Generic function management and cleanup
;;
;; Example usage:
;;
;;   Basic function definition and currying:
;;   (ldef add3 (x y z) (+ x y z))
;;   (funcall (add3 1 2) 3) ; => 6
;;
;;   Pattern matching with types and values:
;;   (ldef greet ((name "Alice")) "Hello, Alice!")
;;   (ldef greet ((name :string)) (concat "Hi, " name "!"))
;;   (greet "Alice") ; => "Hello, Alice!"
;;   (greet "Bob")   ; => "Hi, Bob!"
;;
;;   Rest parameters:
;;   (ldef sum ((nums :rest)) (apply '+ nums))
;;   (sum 1 2 3 4 5) ; => 15
;;
;;   Syntax transformation:
;;   (with-l ((add3 1) 2 3)) ; => 6
;;
;;   Placeholder substitution:
;;   (__ (+ __ (* __ 2)) 5) ; => 15
;;
;;   Partial application:
;;   (funcall (lpartial '+ 10) 5) ; => 15
;;
;;   Lambda with arrow syntax:
;;   (l x y -> (+ x y)) ; => (lambda (x y) (+ x y))
;;
;;   Function composition:
;;   (ldef double (l x -> (* 2 x)))
;;   (ldef add-one (l x -> (+ 1 x)))
;;   (funcall (lcomp add-one double) 5) ; => 11
;;
;;   Documentation annotation:
;;   @doc "Multiplies two numbers."
;;   (ldef multiply (x y) (* x y))
;;
;; The library is designed to be incrementally adoptable - you can use
;; individual features without enabling the full syntax transformation
;; system, or enable l-syntax globally for a complete functional
;; programming experience.
;;
;;; Code:

;; Adding ./lib to load-path
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name "lib" (file-name-directory (or load-file-name buffer-file-name))))

;; internal library paths
(require 'l-load-path)

;; core
(require 'l-main)
(require 'l-syntax)

;; l-mode
(require 'l-mode)

;; utilities
(l-require 'l-function)

(provide 'l)
;;; l.el ends here
