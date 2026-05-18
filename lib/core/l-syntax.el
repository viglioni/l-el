;;; l-syntax.el --- Syntax transformation and evaluation advice for l.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Laura Viglioni

;; Author: Laura Viglioni
;; Keywords: lisp, functional, programming, syntax, transformation
;; URL: https://github.com/viglioni/l-el
;; since: 0.2.0
;; updated-at: (0.3.0)

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
;;
;; Byte-compilation note: files with `l-syntax: t' must be loaded from
;; source.  The load-time transformation runs through
;; `load-read-function', which is not consulted when loading `.elc'
;; files.  Do not byte-compile files that rely on l-syntax.

;;; Code:

(require 'l-main)
(require 'l-mode)
(require 'l-exception)
(defvar l--syntax-loading nil
  "Non-nil while `l--load-file-advice' is delegating to the real `load'.
Guards the advice against re-entering itself when the loaded file (or
its transitive loads) triggers another `load' call, and prevents the
eval advices (`l--eval-last-sexp-advice', `l--eval-region-advice',
`l--eval-buffer-advice') from re-applying the transformation when
`load' calls them internally via `load-with-code-conversion'.

since: NEXT")

(defcustom l-syntax nil
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
setting it to nil provides more granular control on a per-file basis.

Note: l-syntax files must be loaded from source.  The load advice
applies its transformation via `load-read-function', which is not
consulted for `.elc' files.  Do not byte-compile files that declare
`-*- l-syntax: t -*-'.

since: 0.3.0"
  :type 'boolean
  :group 'l
  :safe #'booleanp)

(defun l-syntax-advices ()
  "Add advice to evaluation functions for l syntax support.
This function adds around advice to `eval-last-sexp', `eval-region',
`eval-buffer', `load-file', and `load' to enable l syntax processing.

Enable `l-mode' when `l-syntax' is t.

since: 0.2.0"
  (interactive)
  (add-hook   'after-change-major-mode-hook #'l-mode--auto-enable)
  (advice-add 'eval-last-sexp      :around #'l--eval-last-sexp-advice)
  (advice-add 'eval-region         :around #'l--eval-region-advice)
  (advice-add 'eval-buffer         :around #'l--eval-buffer-advice)
  (advice-add 'load-file           :around #'l--load-file-advice)
  (advice-add 'load                :around #'l--load-file-advice))

(defun l-syntax-remove-advices ()
  "Remove advice to evaluation functions for l syntax support.
This function adds around advice to `eval-last-sexp', `eval-region',
`eval-buffer', `load-file', and `load' to enable l syntax processing.

since: 0.2.0"
  (interactive)
  (remove-hook   'after-change-major-mode-hook #'l-mode--auto-enable)
  (advice-remove 'eval-last-sexp               #'l--eval-last-sexp-advice)
  (advice-remove 'eval-region                  #'l--eval-region-advice)
  (advice-remove 'eval-buffer                  #'l--eval-buffer-advice)
  (advice-remove 'load-file                    #'l--load-file-advice)
  (advice-remove 'load                         #'l--load-file-advice))


(defun l-require (feature &optional filename noerror)
  "Load FEATURE with l-syntax transformations applied.
Resolves FEATURE via `locate-library' (or uses FILENAME if given),
installs the syntax-aware `load-read-function' for the duration of
the load, then delegates to standard `load' so the full load pipeline
(features tracking, `load-history', `load-file-name',
`after-load-functions') runs unchanged.  Works whether or not
`l-syntax-advices' has been installed.

FEATURE is the library name symbol to load.
FILENAME is optional - if provided, load this file instead of searching.
NOERROR - if non-nil, don't signal error if file not found.

Returns FEATURE if successful, nil if NOERROR is non-nil and loading failed.

since: 0.3.0"
  (let* ((feature-name (symbol-name feature))
         (file-to-load (or filename (locate-library feature-name))))
    (cond
     (file-to-load
      (let ((l--syntax-loading t)
            (load-read-function (l--make-syntax-reader)))
        (load file-to-load noerror))
      feature)
     (noerror nil)
     (t (signal 'l-missing-library-error (list feature-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use l syntax without `with-l' ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun l--should-use-l-syntax-p ()
  "Check if l-syntax should be used in current context.
Checks three sources in order:
1. Global l-syntax variable
2. Buffer-local l-syntax variable
3. File-local l-syntax variable (prop-line or local variables section)

since: 0.2.0"
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
ORIG-FUN is `eval-last-sexp'.

since: 0.2.0"
  (if (and (l--should-use-l-syntax-p) (not l--syntax-loading))
      ;; l-syntax is enabled - evaluate the wrapped sexp directly
      (let* ((sexp (elisp--preceding-sexp))
             (processed-sexp (l--process-sexp-for-doc sexp))
             (wrapped-sexp `(with-l ,processed-sexp))
             (result (eval wrapped-sexp)))
        ;; Handle output formatting like the original function
        (let ((eval-last-sexp-arg-internal (car args)))
          (if eval-last-sexp-arg-internal
              (message "%s" result)
            (pp-display-expression result "*Pp Eval Output*")))
        result)
    ;; l-syntax not enabled - use original function
    (apply orig-fun args)))

(defun l--process-sexp-for-doc (sexp)
  "Process SEXP to group @doc expressions if it's a progn.
Returns the processed sexp with @doc expressions grouped.

since: 0.3.0"
  (if (and (consp sexp) (eq (car sexp) 'progn))
      ;; It's a progn - group @doc expressions in the body
      (let ((grouped-body (l--group-doc-expressions (cdr sexp))))
        `(progn ,@grouped-body))
    ;; Not a progn - return as-is
    sexp))

(defun l--eval-region-advice (orig-fun start end &rest args)
  "Advice for `eval-region' to handle l-syntax.
ORIG-FUN is the original load function.
START and END are the region start and end point.
ARGS are additional arguments passed to load.

since: 0.2.0"
  (if (and (l--should-use-l-syntax-p) (not l--syntax-loading))
      ;; l-syntax is enabled - wrap entire region in with-l
      (let* ((region-content (buffer-substring-no-properties start end))
             (grouped-content (l--group-doc-in-content region-content)))
        (eval `(with-l ,(read grouped-content))))
    ;; l-syntax not enabled - use original function
    (apply orig-fun start end args)))

(defun l--eval-buffer-advice (orig-fun &rest args)
  "Advice for `eval-region' to handle l-syntax.
ORIG-FUN is the original load function.
ARGS are additional arguments passed to load.

since: 0.2.0"
  (if (and (l--should-use-l-syntax-p)
           (not l--syntax-loading)
           (not load-in-progress))
      ;; l-syntax is enabled - wrap entire buffer in with-l
      (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
             (grouped-content (l--group-doc-in-content buffer-content)))
        (eval (read (format "(with-l %s)" grouped-content))))
    ;; l-syntax not enabled - use original function
    (apply orig-fun args)))

(defun l--file-wants-l-syntax-p (filename)
  "Return non-nil if FILENAME should be loaded with l-syntax transformations.
Checks the dynamic value of `l-syntax' first (which `l-require'
let-binds to t), then scans the first ~1 KB of FILENAME for a
`-*- l-syntax: t -*-' prop-line.  End-of-file `;; Local Variables:'
forms are not honoured at load time.

since: NEXT"
  (and (file-exists-p filename)
       (string-suffix-p ".el" filename)
       (or (and (boundp 'l-syntax) l-syntax)
           (with-temp-buffer
             (insert-file-contents filename nil 0 1024)
             (goto-char (point-min))
             (and (looking-at "^.*-\\*-.*l-syntax:[ \t]*t.*-\\*-") t)))))

(defun l--syntax-prepare-form (form)
  "Apply the l-syntax transformations to FORM and return the result.
The form is `macroexpand-all'-ed and then run through
`l--transform-curry-calls', matching what `with-l' does per body
form.

since: NEXT"
  (l--transform-curry-calls (macroexpand-all form)))

(defun l--make-syntax-reader ()
  "Return a `load-read-function' implementing the l-syntax reader.
Reads one top-level form from the input stream and applies
`l--syntax-prepare-form'.  When the form is the symbol `@doc',
reads the two following forms and groups them into
`(@doc DOCSTRING NEXT-FORM)' before transforming, preserving the
cross-form grouping that `l--group-doc-expressions' performs on a
whole-file basis.  If end-of-file occurs while reading the lookahead
pair, the bare `@doc' symbol is returned and the next reader call
will surface the EOF naturally.

since: NEXT"
  (lambda (stream)
    (let* ((form (read stream))
           (grouped
            (if (eq form '@doc)
                (condition-case nil
                    (let* ((docstring (read stream))
                           (next-form (read stream)))
                      `(@doc ,docstring ,next-form))
                  (end-of-file form))
              form)))
      (l--syntax-prepare-form grouped))))

(defun l--load-file-advice (orig-fun filename &rest args)
  "Around-advice for `load' / `load-file' enabling l-syntax processing.
When FILENAME is an `.el' file that requests l-syntax (either via the
global / let-bound `l-syntax' value or its prop-line), install a
syntax-aware `load-read-function' and delegate to ORIG-FUN.  The
standard `load' machinery handles `features-being-required',
`load-history', `load-file-name', `after-load-functions', and the
recursion guard unchanged.  ARGS are forwarded verbatim.

The advice short-circuits when `l--syntax-loading' is already t, so a
transitively-loaded file goes through its own decision rather than
inheriting the outer call's reader.

since: 0.2.0"
  (if (or l--syntax-loading
          (not (l--file-wants-l-syntax-p filename)))
      (apply orig-fun filename args)
    (let ((l--syntax-loading t)
          (load-read-function (l--make-syntax-reader)))
      (apply orig-fun filename args))))

(defun l--group-doc-in-content (content)
  "Group @doc expressions in CONTENT string.
Transforms: @doc \"...\" (ldef ...) -> (@doc \"...\" (ldef ...))
Returns the modified content as a string.

since: 0.3.0"
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((forms '()))
      ;; Read all forms
      (while (not (eobp))
        (condition-case nil
            (push (read (current-buffer)) forms)
          (end-of-file nil)))
      
      ;; Group @doc expressions
      (let ((grouped-forms (l--group-doc-expressions (nreverse forms))))
        ;; Convert back to string
        (mapconcat (lambda (form) (format "%S" form)) grouped-forms "\n")))))


(defun l--check-for-doc-before-sexp (sexp)
  "Check if there's a @doc before the current sexp and group them.
Returns the grouped expression or the original SEXP.

since: 0.3.0"
  (save-excursion
    (backward-sexp 1) ; Move to start of current sexp
    (skip-chars-backward " \t\n")
    (backward-sexp 1) ; Try to get previous sexp
    (let ((prev-sexp (ignore-errors (elisp--preceding-sexp))))
      (if (and prev-sexp (stringp prev-sexp))
          ;; Found a string, check if there's @doc before it
          (progn
            (backward-sexp 1)
            (skip-chars-backward " \t\n")
            (backward-sexp 1)
            (let ((doc-symbol (ignore-errors (elisp--preceding-sexp))))
              (if (eq doc-symbol '@doc)
                  `(@doc ,prev-sexp ,sexp)
                sexp)))
        sexp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use l-mode with l-syntax ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun l-mode--auto-enable ()
  "Hook for `emacs-lisp-mode' to auto-enable `l-mode' when `l-syntax' is active.

since: 0.3.0"
  (when (and (eq major-mode 'emacs-lisp-mode)
             (not (eq major-mode 'l-mode))
             (l--should-use-l-syntax-p))
    (l-mode)))


(provide 'l-syntax)
;;; l-syntax.el ends here
