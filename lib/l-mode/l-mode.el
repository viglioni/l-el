;;; l-mode.el --- Major mode for l.el with enhanced syntax highlighting -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laura Viglioni

;; Author: Laura Viglioni
;; Version: 0.2.0
;; Package-Requires: ((emacs "29"))
;; Keywords: lisp, functional, programming, utilities, mode
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

;; l-mode.el provides a major mode for l.el with enhanced syntax highlighting.
;;
;; This mode extends emacs-lisp-mode with additional font-lock keywords
;; specifically designed for l.el's @doc annotations and backtick syntax.
;;
;; Features:
;; - Enhanced highlighting for @doc strings
;; - Special highlighting for backtick expressions within @doc strings
;; - Based on emacs-lisp-mode for full Emacs Lisp support
;;
;; Usage:
;; The mode is automatically enabled when working with l.el files.
;; It can also be manually activated with M-x l-mode.

;;; Code:

(defvar l-additional-keywords
  '(("\\(@doc\\)\\s-+\\(\"\\(?:.\\|\n\\)*?\"\\)"
     (1 font-lock-constant-face)
     (2 font-lock-doc-face t))
    ;; Backticks specifically within @doc strings
    ("\\(@doc\\)\\s-+\\(\"\\(?:[^\"\\]\\|\\\\.\\|\n\\)*?`\\([^'`]+\\)'\\(?:[^\"\\]\\|\\\\.\\|\n\\)*?\"\\)"
     (3 font-lock-constant-face t)))
  "Additional font lock keywords for l-mode.")

(define-derived-mode l-mode emacs-lisp-mode "L"
    "Major mode for l.el syntax with enhanced highlighting."
    :group 'l
    ;; Add keywords with 'prepend to give them higher priority
    (font-lock-add-keywords nil l-additional-keywords 'prepend)
    (font-lock-mode 1)
    (font-lock-ensure))

(provide 'l-mode)

;;; l-mode.el ends here
