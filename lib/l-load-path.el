;;; l-load-path.el --- Load path setup for L library -*- lexical-binding: t; -*-

;; since: 0.3.0
;; updated-at: ()

;;; Code:

(defun l--setup-load-path ()
  "Setup load-path for L library.

since: 0.3.0"
  (let* ((current-file (or load-file-name buffer-file-name))
         (lib-dir (file-name-directory current-file)))
    
    ;; If we're in a subdirectory, go up to find the project root
    (while (and lib-dir 
                (not (file-exists-p (expand-file-name "l.el" lib-dir))))
      (setq lib-dir (file-name-directory (directory-file-name lib-dir))))
    
    (when lib-dir
      ;; Add project root
      (add-to-list 'load-path lib-dir)

      ;; Recursively add every directory under lib/ so files can be
      ;; located by basename regardless of how deep they sit.  Without
      ;; recursion, only direct children of lib/ (e.g. lib/utilities)
      ;; were on `load-path', which left lib/utilities/l-typeclasses/
      ;; unfindable from a plain interactive session.
      (let ((lib-base-dir (expand-file-name "lib" lib-dir)))
        (when (file-directory-p lib-base-dir)
          (l--add-subdirs-recursively lib-base-dir))))))

(defun l--add-subdirs-recursively (dir)
  "Add DIR and all its descendant directories to `load-path'.
Skips hidden entries (those starting with `.').

since: NEXT"
  (add-to-list 'load-path dir)
  (dolist (entry (directory-files dir t "^[^.]"))
    (when (file-directory-p entry)
      (l--add-subdirs-recursively entry))))

;; Setup load-path when this file is loaded
(l--setup-load-path)

(provide 'l-load-path)
;;; l-load-path.el ends here
