;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: 0.3.0
;;
;; l-test-helpers.el:
;; Set up buttercup and some macro helpers.
;;

;;; code:

(require 'l)

;;;###autoload
(defun load-test-file (file-name)
  "Search for FILE-NAME and load it."
  (mapcar 'load
          (directory-files-recursively "." (concat file-name ".el"))))

;;;###autoload
(defmacro context (description &rest body)
  "The same as `describe', but more idiomatic and with defun indentation.
DESCRIPTION is the context description: string.
BODY is the test block body."
  (declare (indent defun))
  `(describe (concat "when " ,description)
             ,@body))

;;;###autoload
(defmacro test-it (description &rest body)
  "The same as `it', but with defun indentation.
DESCRIPTION is the test suit title and
BODY is the test suit block."
  (declare (indent defun))
  `(it (concat "it " ,description)
       ,@body))

;;;###autoload
(defun l-find-test-file (source-file)
  "Find the test file corresponding to SOURCE-FILE.
If SOURCE-FILE is already a test file, return it.
Otherwise, look for the corresponding test file in the test directory.

Examples:
  lib/utilities/l-function.el -> test/utilities/l-function-test.el
  lib/core/l-main.el -> test/core/l-main-test.el"
  (cond
   ;; Already a test file
   ((string-match-p "-test\\.el$" source-file)
    source-file)
   
   ;; Try to find corresponding test file
   (t
    (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                  (projectile-project-root))
                            (locate-dominating-file source-file ".git")
                            default-directory))
           (relative-path (file-relative-name source-file project-root))
           (test-file-name (replace-regexp-in-string
                           "\\.el$" "-test.el"
                           (file-name-nondirectory source-file)))
           (test-path (replace-regexp-in-string
                      "^lib/" "test/"
                      (file-name-directory relative-path)))
           (full-test-path (expand-file-name
                           (concat test-path test-file-name)
                           project-root)))
      (if (file-exists-p full-test-path)
          full-test-path
        (error "Test file not found: %s" full-test-path))))))

;;;###autoload
(defun l-run-current-test-buffer ()
  "Run all tests in the current buffer.
If current buffer is a source file (not a test file), find and run its test file.
If current buffer is a test file, run all tests in it.
Outputs results to a compilation buffer."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (test-file (if current-file
                       (l-find-test-file current-file)
                     (error "Buffer has no associated file")))
         (project-root (or (and (fboundp 'projectile-project-root)
                               (projectile-project-root))
                          (locate-dominating-file test-file ".git")
                          default-directory))
         (default-directory project-root)
         (test-dir (file-name-directory (file-relative-name test-file project-root)))
         (command (format "cask exec buttercup -L . -L lib -L test %s" test-dir)))
    
    (compile command)))

;;;###autoload
(defun l-run-test-at-point ()
  "Run the test suite at point.
If current buffer is a source file, find its test file and run all tests.
If current buffer is a test file, run only the test at point.
Outputs results to a compilation buffer."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (test-file (if current-file
                       (l-find-test-file current-file)
                     (error "Buffer has no associated file")))
         (is-test-file (string= current-file test-file)))
    
    (if is-test-file
        ;; In test file - use buttercup's built-in run-at-point
        (progn
          (unless (featurep 'buttercup)
            (require 'buttercup))
          (save-buffer)
          (eval-buffer)
          (buttercup-run-at-point))
      ;; In source file - run all tests from the test file
      (l-run-current-test-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Isolated IELM Environment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'comint)

(defvar l-isolated-ielm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "RET") 'l-isolated-ielm-send-input)
    (define-key map (kbd "S-<return>") 'newline)
    map)
  "Keymap for L-Isolated-IELM mode.")

(define-derived-mode l-isolated-ielm-mode comint-mode "L-Isolated-IELM"
  "Major mode for interacting with isolated IELM subprocess.
\\{l-isolated-ielm-mode-map}"
  (setq comint-prompt-regexp "^ELISP> \\|^IELM> ")
  (setq comint-use-prompt-regexp t)
  ;; Don't send input on every newline, wait for explicit send
  (setq comint-process-echoes nil)
  (setq-local comint-input-sender 'l-isolated-ielm-input-sender))

(defun l-isolated-ielm-input-sender (proc string)
  "Send STRING to PROC as a complete S-expression."
  ;; Send the string followed by newline
  (comint-send-string proc string)
  (comint-send-string proc "\n"))

(defun l-isolated-ielm-send-input ()
  "Send the current input to the isolated IELM process.
RET sends the input, Shift-RET adds a newline."
  (interactive)
  ;; Use comint's send-input which will use our custom input-sender
  (comint-send-input))

;;;###autoload
(defun l-isolated-ielm ()
  "Start IELM in an isolated Emacs process buffer.
Opens IELM running in a separate Emacs subprocess within the current Emacs.
The subprocess has the project root as default-directory and in load-path.

Keys:
  RET       - Send input to IELM
  S-RET     - Insert newline without sending"
  (interactive)
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                               (projectile-project-root))
                          (locate-dominating-file default-directory ".git")
                          default-directory))
         (buffer-name "*L-Isolated-IELM*"))
    
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    (make-comint-in-buffer
     "l-isolated-ielm"
     buffer-name
     "emacs"
     nil
     "-Q"
     "--batch"
     (format "--eval=(setq default-directory %S)" project-root)
     (format "--eval=(add-to-list 'load-path %S)" project-root)
     (format "--eval=(add-to-list 'load-path %S)" (expand-file-name "lib" project-root))
     (format "--eval=(add-to-list 'load-path %S)" (expand-file-name "lib/core" project-root))
     (format "--eval=(add-to-list 'load-path %S)" (expand-file-name "lib/utilities" project-root))
     "--eval=(require 'ielm)"
     "--eval=(setq lexical-binding t)"
     "--eval=(princ \"*** L-Isolated-IELM Ready ***\\nUse Shift-RET for newlines, RET to evaluate.\\nIELM> \")"
     ;; Simple read-eval-print loop - read from stdin
     "--eval=(while t (condition-case err (let* ((input (read)) (result (eval input lexical-binding))) (prin1 result) (princ \"\\nIELM> \")) (error (princ (format \"Error: %S\\nIELM> \" err)))))")
    
    (with-current-buffer buffer-name
      (l-isolated-ielm-mode))
    
    (pop-to-buffer buffer-name)
    (message "Started isolated IELM with project root: %s" project-root)))

(provide 'l-test-helpers)

;;; l-test-helpers.el ends here.
