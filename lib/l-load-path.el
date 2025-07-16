;;; l-load-path.el --- Load path setup for L library -*- lexical-binding: t; -*-

;;; Code:

(defun l-setup-load-path ()
  "Setup load-path for L library."
  (let* ((current-file (or load-file-name buffer-file-name))
         (lib-dir (file-name-directory current-file)))
    
    ;; If we're in a subdirectory, go up to find the project root
    (while (and lib-dir 
                (not (file-exists-p (expand-file-name "l.el" lib-dir))))
      (setq lib-dir (file-name-directory (directory-file-name lib-dir))))
    
    (when lib-dir
      ;; Add project root
      (add-to-list 'load-path lib-dir)
      
      ;; Add all lib subdirectories
      (let ((lib-base-dir (expand-file-name "lib" lib-dir)))
        (when (file-directory-p lib-base-dir)
          (dolist (subdir (directory-files lib-base-dir t "^[^.]"))
            (when (file-directory-p subdir)
              (add-to-list 'load-path subdir))))))))

;; Setup load-path when this file is loaded
(l-setup-load-path)

(provide 'l-load-path)
;;; l-load-path.el ends here
