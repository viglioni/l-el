;;; l-mode.el --- Major mode for l.el with enhanced syntax highlighting -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laura Viglioni

;; Author: Laura Viglioni
;; Version: NEXT
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

(defconst l-sexp-rx
  (rx "("
      (* (or
          ;; Regular characters (not parens, quotes, backslashes)
          (not (any "()\"\\"))
          ;; Escaped characters
          (seq "\\" anything)
          ;; Quoted strings with proper escaping
          (seq "\""
               (* (or (not (any "\"\\"))
                      (seq "\\" anything)))
               "\"")
          ;; One level of nested parentheses
          (seq "("
               (* (or (not (any "()\"\\"))
                      (seq "\\" anything)
                      (seq "\"" (* (or (not (any "\"\\")) (seq "\\" anything))) "\"")))
               ")")))
      ")"))

(defun l-highlight-sexps-in-docstring (limit)
  "Font-lock matcher for s-expressions within docstrings up to LIMIT."
  (let (match-found)
    (while (and (not match-found) 
                (re-search-forward "(" limit t))
      (when (l-mode-point-in-docstring-p)
        (let ((start (match-beginning 0)))
          (condition-case nil
              (progn
                (goto-char start)
                (forward-sexp 1)
                (set-match-data (list start (point)))
                (setq match-found t))
            (error 
             ;; If forward-sexp fails, advance past the problematic "(" 
             ;; to avoid infinite loop
             (goto-char (1+ start)))))))
    match-found))

(defun l-mode-point-in-docstring-p ()
  "Return non-nil if point is inside a docstring."
  (let ((face (get-text-property (point) 'face)))
    (or (eq face 'font-lock-doc-face)
        (and (listp face) (memq 'font-lock-doc-face face)))))


(defvar l-mode-additional-keywords
  `(;; @doc keyword highlighting
    ("\\(@doc\\)\\s-+\\(\"\\(?:[^\"\\]\\|\\\\.\\)*\"\\)"
     (1 font-lock-constant-face)
     (2 font-lock-doc-face))

    ;; Strings within ANY docstring (escaped quotes)
    ("\\(\\\\\"\\(?:[^\\\"\\\\]\\|\\\\.\\)*\\\\\"\\)"
     (1 (when (save-match-data (l-mode-point-in-docstring-p))
          font-lock-string-face)
        prepend))
    
    (l-highlight-sexps-in-docstring
     (0 (when (save-match-data (l-mode-point-in-docstring-p))
          font-lock-variable-name-face)
        prepend))

    ;; Backslashes within ANY docstring
    ("\\(\\\\\\)"
     (1 (when (save-match-data (l-mode-point-in-docstring-p))
          'shadow)
        prepend)))
  "Additional font lock keywords for `l-mode'.")

(defun l-font-lock-syntactic-face-function (state)
  "Determine syntactic face for position based on STATE.
This function handles @doc strings specially, treating them as docstrings."
  (if (nth 3 state)  ; we're in a string
      (save-excursion
        (goto-char (nth 8 state))  ; go to string start
        (if (looking-back "@doc\\s-+" (line-beginning-position))
            font-lock-doc-face
          font-lock-string-face))
    (lisp-font-lock-syntactic-face-function state)))

(define-derived-mode l-mode emacs-lisp-mode "L"
  "Major mode for l.el syntax with enhanced highlighting."
  ;; Add our keywords
  (font-lock-add-keywords nil l-mode-additional-keywords 'prepend)
  
  ;; Use our custom syntactic face function
  (setq-local font-lock-syntactic-face-function #'l-font-lock-syntactic-face-function)
  
  ;; Ensure font-lock is active
  (font-lock-mode 1)
  (font-lock-ensure))



(provide 'l-mode)

;;; l-mode.el ends here
