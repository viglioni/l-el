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
;; This library introduces currying, partial application, pattern matching,
;; and placeholder substitution utilities that make Emacs Lisp more expressive
;; and closer to modern functional programming languages.
;;
;; Key features:
;; - Automatic currying with `ldef'
;; - Pattern matching with `ldef'
;; - Type matching with `ldef'
;; - Partial application with `lpartial'
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
;;   (funcall (lpartial '+ 10) 5) ; => 15
;;
;; Check functions for more documentation.
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

(provide 'l)
;;; l.el ends here
