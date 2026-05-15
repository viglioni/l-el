;;; release.el --- Script for releasing new versions -*- lexical-binding: t; -*-

;;; Commentary:

;; Cuts a new release.  Bumps version metadata across the package, commits,
;; tags, and (optionally) pushes.
;;
;; Metadata model
;; --------------
;;
;; File-level header annotations live in the comment block at the top of each
;; in-scope `.el' file:
;;
;;     ;; Version: 1.1.0          ; package version (only on l.el / l-mode.el)
;;     ;; since: 1.1.0             ; resolved once when the file was introduced
;;     ;; updated-at: (1.2.0)      ; appended to on each release where the file changed
;;
;; In-flight placeholders before a release:
;;
;;     ;; since: NEXT
;;     ;; updated-at: ()
;;
;; Function-level annotations live inside `defun'/`defmacro' docstrings:
;;
;;     "since: 1.1.0"        ; frozen
;;     "since: NEXT"          ; in-flight, resolved by this script
;;
;; What the script touches
;; -----------------------
;;
;; In every in-scope `.el' file (l.el + everything under lib/):
;;   - Replaces every `since: NEXT' with `since: NEW-VERSION'.
;;   - If a `;; Version: CURRENT-VERSION' line exists, rewrites it to NEW-VERSION.
;;   - If the file changed since CURRENT-VERSION's tag (ignoring the
;;     `;; updated-at:' line itself), appends NEW-VERSION to its `updated-at' list.
;;
;; Plus: Cask, readme.org, the `l-version' defun in l.el, the version
;; literal asserted by `test/l-test.el', and the changelog.
;;
;; Safety
;; ------
;;
;; - Working tree must be clean before starting.
;; - Only files the script actually modified are `git add'ed (no `-A').
;; - Every git call checks its exit code.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defconst l-release--semver-rx "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'"
  "Strict semver match: MAJOR.MINOR.PATCH only.")

;;;;;;;;;;;;;;;;;;;;;;;
;;; Pure helpers
;;;;;;;;;;;;;;;;;;;;;;;

(defun l-release--bump-version (current-version increment-type)
  "Return CURRENT-VERSION bumped by INCREMENT-TYPE (\"major\"/\"minor\"/\"patch\")."
  (unless (string-match-p l-release--semver-rx current-version)
    (error "Invalid current version %S; expected MAJOR.MINOR.PATCH" current-version))
  (let* ((parts (mapcar #'string-to-number (split-string current-version "\\.")))
         (major (nth 0 parts))
         (minor (nth 1 parts))
         (patch (nth 2 parts)))
    (pcase increment-type
      ("major" (format "%d.0.0" (1+ major)))
      ("minor" (format "%d.%d.0" major (1+ minor)))
      ("patch" (format "%d.%d.%d" major minor (1+ patch)))
      (_ (error "Unknown increment type: %s" increment-type)))))

(defun l-release--resolve-since-next-in-string (str new-version)
  "Return STR with every `since: NEXT' replaced by `since: NEW-VERSION'.
Covers both the file-level header form (`;; since: NEXT') and the
function-level docstring form (`since: NEXT')."
  (replace-regexp-in-string
   "\\bsince: NEXT\\b"
   (format "since: %s" new-version)
   str t t))

(defun l-release--update-version-header (str current-version new-version)
  "Return STR with `;; Version: CURRENT-VERSION' replaced by `;; Version: NEW-VERSION'.
Only the line matching CURRENT-VERSION is rewritten; other `;; Version:'
lines (if any) are left alone."
  (replace-regexp-in-string
   (format ";; Version: %s\\b" (regexp-quote current-version))
   (format ";; Version: %s" new-version)
   str t t))

(defun l-release--append-updated-at (str new-version)
  "Append NEW-VERSION to STR's `;; updated-at:' list.
Returns a cons (NEW-STR . MODIFIED-P).  MODIFIED-P is nil if no
`;; updated-at:' line was found (caller should warn)."
  (let ((rx (rx line-start
                ";; updated-at:" (* space)
                "(" (group (* (not (any ")" "\n")))) ")"
                (* space) line-end)))
    (if (string-match rx str)
        ;; Capture group + match positions BEFORE doing anything that might
        ;; clobber `match-data' (e.g. `split-string', `string-trim').
        (let* ((inside (match-string 1 str))
               (md (match-data))
               (existing (split-string inside "[ \t]+" t))
               (combined (append existing (list new-version)))
               (replacement (format ";; updated-at: (%s)"
                                    (mapconcat #'identity combined " "))))
          (set-match-data md)
          (cons (replace-match replacement t t str) t))
      (cons str nil))))

(defun l-release--rewrite-changelog (content new-version today)
  "Return CONTENT with `* Unreleased' replaced by an Unreleased-then-release block."
  (unless (string-match "^\\* Unreleased$" content)
    (error "No `* Unreleased' section in changelog"))
  (replace-regexp-in-string
   "^\\* Unreleased$"
   (format "* Unreleased\n\n* %s - %s" new-version today)
   content t t))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Git helpers
;;;;;;;;;;;;;;;;;;;;;;;

(defun l-release--git (&rest args)
  "Run git with ARGS.  Return its exit code.  Output -> *l-release-git*."
  (apply #'process-file "git" nil
         (get-buffer-create "*l-release-git*") nil args))

(defun l-release--git-output (&rest args)
  "Run git with ARGS.  Return trimmed stdout; signal on non-zero exit."
  (with-temp-buffer
    (let ((rc (apply #'process-file "git" nil t nil args)))
      (unless (zerop rc)
        (error "git %s failed (rc=%d): %s"
               (mapconcat #'identity args " ") rc (buffer-string)))
      (string-trim (buffer-string)))))

(defun l-release--file-changed-since-tag-p (file tag)
  "Non-nil iff FILE has non-`;; updated-at:' changes between TAG and HEAD."
  (let ((rc (l-release--git "diff" "--quiet"
                            "-I" "^;; updated-at:"
                            tag "--" file)))
    (cond
     ((= rc 0) nil)
     ((= rc 1) t)
     (t (error "git diff failed (rc=%d) for %s" rc file)))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; File walkers / rewriters
;;;;;;;;;;;;;;;;;;;;;;;

(defun l-release--package-elisp-files (root)
  "Return absolute paths of in-scope `.el' files under ROOT.
Includes `l.el' at ROOT and everything under `lib/'.  Test files, scripts,
straight/, .cask/, and .git/ are excluded."
  (let ((files '())
        (l-root (expand-file-name "l.el" root))
        (lib-dir (expand-file-name "lib" root)))
    (when (file-exists-p l-root)
      (push l-root files))
    (when (file-directory-p lib-dir)
      (setq files
            (append files
                    (cl-remove-if
                     (lambda (f) (string-match-p "/\\.\\(cask\\|git\\)/" f))
                     (directory-files-recursively lib-dir "\\.el\\'")))))
    (nreverse files)))

(defun l-release--read-file (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun l-release--write-file (file content)
  (write-region content nil file nil 'no-message))

(defun l-release--rewrite-elisp-file (file current-version new-version)
  "Apply all release rewrites to FILE.  Returns t iff FILE was modified."
  (let* ((orig (l-release--read-file file))
         (after orig))
    (setq after (l-release--resolve-since-next-in-string after new-version))
    (setq after (l-release--update-version-header after current-version new-version))
    (when (l-release--file-changed-since-tag-p file current-version)
      (pcase-let ((`(,next . ,modified) (l-release--append-updated-at after new-version)))
        (cond
         (modified
          (setq after next))
         (t
          (message "WARN: %s changed since %s but has no `;; updated-at:' line; skipping"
                   (file-relative-name file) current-version)))))
    (unless (string= orig after)
      (l-release--write-file file after)
      t)))

(defun l-release--rewrite-cask (file current-version new-version)
  "Rewrite the version literal in Cask FILE.  Returns t iff modified."
  (let* ((orig (l-release--read-file file))
         (rx (format "(package \"l\\.el\" \"%s\"" (regexp-quote current-version)))
         (after (replace-regexp-in-string
                 rx
                 (format "(package \"l.el\" \"%s\"" new-version)
                 orig t t)))
    (unless (string= orig after)
      (l-release--write-file file after)
      t)))

(defun l-release--rewrite-readme (file new-version)
  "Rewrite `:tag \"vX.Y.Z\"' in readme FILE.  Returns t iff modified."
  (let* ((orig (l-release--read-file file))
         (after (replace-regexp-in-string
                 ":tag \"v[0-9]+\\.[0-9]+\\.[0-9]+\""
                 (format ":tag \"v%s\"" new-version)
                 orig t t)))
    (unless (string= orig after)
      (l-release--write-file file after)
      t)))

(defun l-release--rewrite-l-version (file new-version)
  "Rewrite the version literal returned by `l-version' in FILE.
Returns t iff modified.  Matches loosely so formatting changes to the
defun (extra whitespace, edited docstring) don't break it.

Uses an explicit \"[ \\t\\n\\r]\" whitespace class rather than
\"[[:space:]]\" because the latter is syntax-table based and does not
match newlines/tabs in `string-match' outside a buffer context."
  (let* ((orig (l-release--read-file file))
         (rx "\\((defun l-version\\b[^\"]*\"[^\"]*\"[ \t\n\r]*\\)\"[0-9]+\\.[0-9]+\\.[0-9]+\"")
         (after (replace-regexp-in-string
                 rx
                 (format "\\1\"%s\"" new-version)
                 orig)))
    (unless (string= orig after)
      (l-release--write-file file after)
      t)))

(defun l-release--rewrite-l-version-test (file new-version)
  "Rewrite the version literal asserted by the umbrella smoke test in FILE.
The smoke test pins `l-version' to the current release; that pin must be
bumped in lockstep with the release.  Returns t iff modified.

Matches `(l-version) :to-equal \"X.Y.Z\"' tolerantly w.r.t. whitespace so
formatting tweaks don't silently disable the rewrite."
  (let* ((orig (l-release--read-file file))
         (rx "\\((l-version)[ \t\n\r]*:to-equal[ \t\n\r]*\\)\"[0-9]+\\.[0-9]+\\.[0-9]+\"")
         (after (replace-regexp-in-string
                 rx
                 (format "\\1\"%s\"" new-version)
                 orig)))
    (unless (string= orig after)
      (l-release--write-file file after)
      t)))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Orchestrator
;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun release-version (increment-type)
  "Cut a release, bumping the current tag by INCREMENT-TYPE.
INCREMENT-TYPE is one of \"major\", \"minor\", \"patch\".

Operates from the git repo containing `default-directory'.  In batch mode
(`noninteractive') all y-or-n confirmations are skipped."
  (interactive
   (list (completing-read "Increment type: " '("major" "minor" "patch") nil t)))

  (let* ((repo-root (locate-dominating-file default-directory ".git"))
         (default-directory (or (and repo-root (expand-file-name repo-root))
                                (user-error "Not in a git repo")))
         (_ (unless (string-empty-p (l-release--git-output "status" "--porcelain"))
              (user-error "Working tree is not clean; commit or stash first")))
         (current-version (l-release--git-output "describe" "--tags" "--abbrev=0"))
         (_ (unless (string-match-p l-release--semver-rx current-version)
              (user-error "Current tag %S is not strict semver" current-version)))
         (new-version (l-release--bump-version current-version increment-type))
         (today (format-time-string "%Y-%m-%d"))
         (modified-files '()))

    (message "Preparing release: %s -> %s" current-version new-version)
    (unless noninteractive
      (unless (y-or-n-p (format "Bump %s -> %s? " current-version new-version))
        (user-error "Release aborted (no changes made)")))

    ;; .el files
    (dolist (file (l-release--package-elisp-files default-directory))
      (when (l-release--rewrite-elisp-file file current-version new-version)
        (push file modified-files)
        (message "Updated %s" (file-relative-name file))))

    ;; Cask
    (when (and (file-exists-p "Cask")
               (l-release--rewrite-cask "Cask" current-version new-version))
      (push (expand-file-name "Cask") modified-files)
      (message "Updated Cask"))

    ;; readme.org
    (when (and (file-exists-p "readme.org")
               (l-release--rewrite-readme "readme.org" new-version))
      (push (expand-file-name "readme.org") modified-files)
      (message "Updated readme.org"))

    ;; l-version function in l.el (separate from the .el header rewrite path
    ;; because it touches the function body, not the header).
    (let ((lfile "l.el"))
      (when (and (file-exists-p lfile)
                 (l-release--rewrite-l-version lfile new-version))
        (cl-pushnew (expand-file-name lfile) modified-files :test #'string=)
        (message "Updated l-version function in %s" lfile)))

    ;; l-version smoke test (pins `(l-version)' to the current release so a
    ;; missed bump fails CI rather than silently shipping a stale version).
    (let ((tfile "test/l-test.el"))
      (when (and (file-exists-p tfile)
                 (l-release--rewrite-l-version-test tfile new-version))
        (cl-pushnew (expand-file-name tfile) modified-files :test #'string=)
        (message "Updated l-version assertion in %s" tfile)))

    ;; Changelog
    (let ((changelog (cond ((file-exists-p "changelog.org") "changelog.org")
                           ((file-exists-p "CHANGELOG.org") "CHANGELOG.org")
                           ((file-exists-p "CHANGELOG.md") "CHANGELOG.md"))))
      (unless changelog
        (user-error "No changelog file found"))
      (let* ((content (l-release--read-file changelog))
             (new-content (l-release--rewrite-changelog content new-version today)))
        (unless (string= content new-content)
          (l-release--write-file changelog new-content)
          (push (expand-file-name changelog) modified-files)
          (message "Updated %s" changelog))))

    (when (null modified-files)
      (user-error "No files changed by release script — aborting"))

    ;; Commit + tag
    (message "Files to commit:\n  %s"
             (mapconcat #'file-relative-name modified-files "\n  "))
    (unless noninteractive
      (unless (y-or-n-p "Commit and tag? ")
        (user-error
         "Aborted after rewrites.  Revert with: git checkout -- %s"
         (mapconcat (lambda (f) (shell-quote-argument (file-relative-name f)))
                    modified-files " "))))

    (unless (zerop (apply #'l-release--git "add" "--" modified-files))
      (user-error "git add failed"))
    (unless (zerop (l-release--git "commit" "-m"
                                   (format "Release version %s" new-version)))
      (user-error "git commit failed"))
    (message "Committed")

    (unless (zerop (l-release--git "tag" "-a" new-version
                                   "-m" (format "Release %s" new-version)))
      (user-error "git tag failed"))
    (message "Tagged %s" new-version)

    ;; Push
    (unless noninteractive
      (unless (y-or-n-p (format "Push branch + tag %s to origin? " new-version))
        (user-error "Tagged locally; push manually when ready")))

    (unless (zerop (l-release--git "push"))
      (user-error "git push failed"))
    (message "Pushed branch")

    (unless (zerop (l-release--git "push" "origin" new-version))
      (user-error "git push tag failed"))
    (message "Pushed tag %s" new-version)

    (message "Release %s completed!" new-version)))

(provide 'release)

;;; release.el ends here
