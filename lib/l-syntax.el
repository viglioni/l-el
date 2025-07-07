;;; l-syntax.el --- Syntax transformation and evaluation advice for l.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laura Viglioni

;; Author: Laura Viglioni
;; Keywords: lisp, functional, programming, syntax, transformation
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

;; This module provides syntax transformation capabilities for the l.el library,
;; enabling automatic application of l-syntax transformations during evaluation
;; operations without requiring explicit `with-l' wrapping.
;;
;; Key features:
;; - File-local variable support for `l-syntax' control
;; - Advice functions for standard evaluation operations
;; - Automatic syntax transformation when `l-syntax' is enabled
;; - Support for both global and per-file l-syntax configuration
;;
;; The module intercepts calls to `eval-last-sexp', `eval-region', `eval-buffer',
;; `load-file', and `load' to automatically wrap expressions in `with-l' when
;; the `l-syntax' variable is enabled, either globally or as a file-local variable.
;;
;; Usage:
;;   Apply the execution advices:
;;   (l-syntax-advices)
;; Enable l-syntax globally:
;;   (setq l-syntax t)
;;
;; Enable l-syntax per-file using a property line:
;;   ;; -*- l-syntax: t; -*-
;;
;; Or using local variables at the end of the file:
;;   ;; Local Variables:
;;   ;; l-syntax: t
;;   ;; End:
;;
;; The advice functions preserve the original behavior and output formatting
;; of the intercepted functions while transparently applying l-syntax
;; transformations when appropriate.

;;; Code:

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use l syntax without `with-l' ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun l--should-use-l-syntax-p ()
  "Check if l-syntax should be used in current context.
Checks three sources in order:
1. Global l-syntax variable
2. Buffer-local l-syntax variable
3. File-local l-syntax variable (prop-line or local variables section)"
  (or
   ;; 1. Check global l-syntax
   (and (boundp 'l-syntax) l-syntax)
   
   ;; 2. Check buffer-local l-syntax
   (and (local-variable-p 'l-syntax) l-syntax)
   
   ;; 3. Check file-local l-syntax
   (progn
     ;; Process file-local variables in current buffer
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (point-min))
         ;; Process prop-line manually if it exists
         (when (looking-at "^.*-\\*-.*l-syntax:[ \t]*\\([^;]+\\).*-\\*-")
           (let ((value (match-string 1)))
             (when (string-match "t" value)
               (setq-local l-syntax t))))
         ;; Also try the standard hack-local-variables approach
         (ignore-errors
           (hack-local-variables-prop-line)
           (hack-local-variables))))
     (and (boundp 'l-syntax) l-syntax))))

(defun l--eval-last-sexp-advice (orig-fun &rest args)
  "Advice for `eval-last-sexp' to handle l-syntax.
Uses &rest ARGS to handle all possible argument combinations.
ORIG-FUN is `eval-last-sexp'."
  (if (l--should-use-l-syntax-p)
      ;; l-syntax is enabled - evaluate the wrapped sexp directly
      (let* ((sexp (elisp--preceding-sexp))
             (wrapped-sexp `(with-l ,sexp))
             (result (eval wrapped-sexp)))
        ;; Handle output formatting like the original function
        (let ((eval-last-sexp-arg-internal (car args)))
          (if eval-last-sexp-arg-internal
              (message "%s" result)
            (pp-display-expression result "*Pp Eval Output*")))
        result)
    ;; l-syntax not enabled - use original function
    (apply orig-fun args)))

(defun l--eval-region-advice (orig-fun start end &rest args)
  "Advice for `eval-region' to handle l-syntax.
ORIG-FUN is the original load function.
START and END are the region start and end point.
ARGS are additional arguments passed to load."
  (if (l--should-use-l-syntax-p)
      ;; l-syntax is enabled - wrap entire region in with-l
      (let ((region-content (buffer-substring-no-properties start end)))
        (eval `(with-l ,(read region-content))))
    ;; l-syntax not enabled - use original function
    (apply orig-fun start end args)))

(defun l--eval-buffer-advice (orig-fun &rest args)
  "Advice for `eval-region' to handle l-syntax.
ORIG-FUN is the original load function.
ARGS are additional arguments passed to load."
  (if (l--should-use-l-syntax-p)
      ;; l-syntax is enabled - wrap entire region in with-l
      (let ((region-content (buffer-substring-no-properties (point-min) (point-max))))
        (eval `(with-l ,(read region-content))))
    ;; l-syntax not enabled - use original function
    (apply orig-fun args)))

(defun l--load-file-advice (orig-fun filename &rest args)
  "Advice for `load' and `load-file' to handle l-syntax.
Only processes .el files, .elc files are loaded normally.
ORIG-FUN is the original load function.
FILENAME is the file to load.
ARGS are additional arguments passed to load."
  (if (and (file-exists-p filename) (string-suffix-p ".el" filename))
      (with-temp-buffer
        (insert-file-contents filename)
        (if (l--should-use-l-syntax-p)
            ;; l-syntax enabled - wrap and evaluate
            (let ((file-content (buffer-string)))
              (eval (read (format "(with-l %s)" file-content))))
          ;; l-syntax not enabled - use original function
          (apply orig-fun filename args)))
    ;; Not an .el file or doesn't exist - use original function
    (apply orig-fun filename args)))

(provide 'l-syntax)
;;; l-syntax.el ends here
