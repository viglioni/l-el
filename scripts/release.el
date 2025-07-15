;;; release.el --- Script for releasing new versions -*- lexical-binding: t; -*-

(require 'subr-x)

(defun release-version (increment-type)
  "Release a new version, incrementing by INCREMENT-TYPE (major, minor, or patch)."
  (interactive (list (completing-read "Increment type: " '("major" "minor" "patch"))))
  
  ;; Get current version from git tag
  (let* ((current-version (string-trim (shell-command-to-string "git describe --tags --abbrev=0")))
         (version-parts (mapcar #'string-to-number (split-string current-version "\\.")))
         (major (nth 0 version-parts))
         (minor (nth 1 version-parts))
         (patch (nth 2 version-parts))
         new-version)
    
    ;; Calculate new version
    (cond
     ((string= increment-type "major")
      (setq new-version (format "%d.0.0" (1+ major))))
     ((string= increment-type "minor")
      (setq new-version (format "%d.%d.0" major (1+ minor))))
     ((string= increment-type "patch")
      (setq new-version (format "%d.%d.%d" major minor (1+ patch))))
     (t (error "Unknown increment type: %s" increment-type)))
    
    ;; Confirm versions
    (unless (y-or-n-p (format "Confirm current version: %s?" current-version))
      (shell-command "git stash")
      (user-error "Release aborted"))
    
    (unless (y-or-n-p (format "Confirm new version: %s?" new-version))
      (shell-command "git stash")
      (user-error "Release aborted"))
    
    (message "Preparing release: %s -> %s" current-version new-version)
    
    ;; Update "since NEXT" to new version in all .el files
    (dolist (file (directory-files-recursively default-directory "\\.el$"))
      (with-temp-buffer
        (insert-file-contents file)
        (when (search-forward ";; since 0.1.0" nil t)
          (replace-match (format ";; since %s" new-version))
          (write-region (point-min) (point-max) file)
          (message "Updated %s" file))))
    
    ;; Update version in l.el file
    (let ((l-file "l.el"))
      (when (file-exists-p l-file)
        (with-temp-buffer
          (insert-file-contents l-file)
          (goto-char (point-min))
          (when (re-search-forward ";; Version: [0-9]+\\.[0-9]+\\.[0-9]+" nil t)
            (replace-match (format ";; Version: %s" new-version))
            (write-region (point-min) (point-max) l-file)
            (message "Updated %s" l-file)))))
    
    ;; Update tag in readme.org
    (let ((readme-file "readme.org"))
      (when (file-exists-p readme-file)
        (with-temp-buffer
          (insert-file-contents readme-file)
          (goto-char (point-min))
          (when (re-search-forward ":tag \"v[0-9]+\\.[0-9]+\\.[0-9]+\"" nil t)
            (replace-match (format ":tag \"v%s\"" new-version))
            (write-region (point-min) (point-max) readme-file)
            (message "Updated %s" readme-file)))))

    ;; Update version in Cask file
    (let ((cask-file "Cask"))
      (when (file-exists-p cask-file)
        (with-temp-buffer
          (insert-file-contents cask-file)
          (goto-char (point-min))
          (when (re-search-forward "(package \"l\\.el\" \"[0-9]+\\.[0-9]+\\.[0-9]+\"" nil t)
            (replace-match (format "(package \"l.el\" \"%s\"" new-version))
            (write-region (point-min) (point-max) cask-file)
            (message "Updated %s" cask-file)))))

    ;; Update changelog
    (let ((changelog-file (if (file-exists-p "CHANGELOG.org") 
                              "CHANGELOG.org"
                            "CHANGELOG.md"))
          (today (format-time-string "%Y-%m-%d"))
          (unreleased-header (if (file-exists-p "CHANGELOG.org") 
                                 "* Unreleased"
                               "## [Unreleased]")))
      
      (with-temp-buffer
        (insert-file-contents changelog-file)
        
        ;; Replace Unreleased with new version
        (goto-char (point-min))
        (when (re-search-forward (if (file-exists-p "CHANGELOG.org") 
                                     "\\* Unreleased"
                                   "## \\[Unreleased\\]") nil t)
          (replace-match (if (file-exists-p "CHANGELOG.org")
                             (format "* %s - %s" new-version today)
                           (format "## [%s] - %s" new-version today))))
        
        ;; Add new Unreleased section
        (goto-char (point-min))
        (forward-line 4)     ; Move past title and initial description
        (insert unreleased-header "\n\n")
        
        (write-region (point-min) (point-max) changelog-file)
        (message "Updated %s" changelog-file)))
    
    ;; Commit the changes
    (unless (y-or-n-p "Commit changelog and version updates?")
      (shell-command "git stash")
      (user-error "Release aborted"))
    (shell-command (format "git add -A && git commit -m \"Release version %s\"" new-version))
    (message "Changes committed")
    
    ;; Create git tag
    (unless (y-or-n-p (format "Create tag %s?" new-version))
      (shell-command "git stash")
      (user-error "Release aborted"))
    (shell-command (format "git tag -a %s -m \"Release %s\"" new-version new-version))
    (message "Tagged version %s" new-version)
    
    ;; Push changes to remote
    (unless (y-or-n-p "Push changes to remote?")
      (shell-command "git stash")
      (user-error "Release aborted"))
    (shell-command "git push")
    (message "Changes pushed to remote")
    
    ;; Push tag to remote
    (unless (y-or-n-p (format "Push tag %s to remote?" new-version))
      (shell-command "git stash")
      (user-error "Release aborted"))
    (shell-command (format "git push origin %s" new-version))
    (message "Tag %s pushed to remote" new-version)
    
    (message "Release %s completed!" new-version)))

(provide 'release)
