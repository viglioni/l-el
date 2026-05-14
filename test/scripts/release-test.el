;;; -*- lexical-binding: t; -*-
;;
;; @author Laura Viglioni
;; 2026
;;
;; GNU Public License 3.0
;;
;; release-test.el:
;; Unit tests for the pure helpers in scripts/release.el.
;; The orchestrator `release-version' is exercised manually (it shells out
;; to git, tags, pushes — too stateful to unit-test cleanly).

;;; code:

(require 'l-test-helpers)

;; Load release.el from the project's scripts/ directory.  l-test-helpers
;; has already added the project root to `load-path' transitively via
;; l-load-path, but `scripts/' is not on it — so use an explicit path.
;; Find the project root by walking up to the directory containing `l.el'
;; (rather than `scripts/', since `test/scripts/' would shadow it).
(load (expand-file-name
       "scripts/release.el"
       (locate-dominating-file
        (or load-file-name buffer-file-name default-directory)
        "l.el"))
      nil 'no-message)

(describe "release.el"

  (describe "l-release--bump-version"
    (test-it "bumps major and resets minor + patch"
             (expect (l-release--bump-version "1.2.3" "major")
                     :to-equal "2.0.0"))

    (test-it "bumps minor and resets patch"
             (expect (l-release--bump-version "1.2.3" "minor")
                     :to-equal "1.3.0"))

    (test-it "bumps patch"
             (expect (l-release--bump-version "1.2.3" "patch")
                     :to-equal "1.2.4"))

    (test-it "errors on non-semver input"
             (expect (l-release--bump-version "v1.2.3" "patch") :to-throw))

    (test-it "errors on unknown increment type"
             (expect (l-release--bump-version "1.2.3" "bogus") :to-throw)))

  (describe "l-release--resolve-since-next-in-string"
    (test-it "resolves file-level `;; since: NEXT'"
             (let* ((src ";; since: NEXT\n")
                    (out (l-release--resolve-since-next-in-string src "1.2.0")))
               (expect out :to-equal ";; since: 1.2.0\n")))

    (test-it "resolves function-level `since: NEXT' inside a docstring"
             (let* ((src "(defun foo ()\n  \"Do a thing.\n\nsince: NEXT\"\n  nil)\n")
                    (out (l-release--resolve-since-next-in-string src "1.2.0")))
               (expect out :to-equal
                       "(defun foo ()\n  \"Do a thing.\n\nsince: 1.2.0\"\n  nil)\n")))

    (test-it "leaves resolved historical versions untouched"
             ;; This is THE regression: existing `since: 1.0.0' must not be
             ;; rewritten on each release.
             (let* ((src ";; since: 1.0.0\n\"since: 0.5.0\"\n")
                    (out (l-release--resolve-since-next-in-string src "1.2.0")))
               (expect out :to-equal src)))

    (test-it "resolves every occurrence in the buffer"
             (let* ((src ";; since: NEXT\n\n\"docstring\n\nsince: NEXT\"\n")
                    (out (l-release--resolve-since-next-in-string src "1.2.0")))
               (expect out :to-equal
                       ";; since: 1.2.0\n\n\"docstring\n\nsince: 1.2.0\"\n")))

    (test-it "doesn't catch `NEXTREL' or other words that start with NEXT"
             (let* ((src ";; since: NEXTREL\n")
                    (out (l-release--resolve-since-next-in-string src "1.2.0")))
               (expect out :to-equal src))))

  (describe "l-release--update-version-header"
    (test-it "rewrites `;; Version: <current>' to <new>"
             (let* ((src ";; Version: 1.1.0\n")
                    (out (l-release--update-version-header src "1.1.0" "1.2.0")))
               (expect out :to-equal ";; Version: 1.2.0\n")))

    (test-it "leaves other `;; Version:' lines alone"
             ;; A stale Version line from a different (e.g. forked) component
             ;; shouldn't get bumped just because we're releasing.
             (let* ((src ";; Version: 0.5.0\n;; Version: 1.1.0\n")
                    (out (l-release--update-version-header src "1.1.0" "1.2.0")))
               (expect out :to-equal ";; Version: 0.5.0\n;; Version: 1.2.0\n")))

    (test-it "no-op when current version doesn't appear"
             (let* ((src ";; Version: 9.9.9\n")
                    (out (l-release--update-version-header src "1.1.0" "1.2.0")))
               (expect out :to-equal src))))

  (describe "l-release--append-updated-at"
    (test-it "appends to an empty list"
             (let* ((src ";; updated-at: ()\n")
                    (result (l-release--append-updated-at src "1.2.0")))
               (expect (car result) :to-equal ";; updated-at: (1.2.0)\n")
               (expect (cdr result) :to-be-truthy)))

    (test-it "appends to a non-empty list"
             (let* ((src ";; updated-at: (1.0.0 1.1.0)\n")
                    (result (l-release--append-updated-at src "1.2.0")))
               (expect (car result) :to-equal
                       ";; updated-at: (1.0.0 1.1.0 1.2.0)\n")
               (expect (cdr result) :to-be-truthy)))

    (test-it "signals not-modified when no `;; updated-at:' line is present"
             (let* ((src ";; Version: 1.1.0\n")
                    (result (l-release--append-updated-at src "1.2.0")))
               (expect (car result) :to-equal src)
               (expect (cdr result) :to-equal nil)))

    (test-it "tolerates extra whitespace inside the parens"
             (let* ((src ";; updated-at: ( 1.0.0   1.1.0 )\n")
                    (result (l-release--append-updated-at src "1.2.0")))
               (expect (car result) :to-equal
                       ";; updated-at: (1.0.0 1.1.0 1.2.0)\n"))))

  (describe "l-release--rewrite-changelog"
    (test-it "replaces `* Unreleased' with new-version header and re-inserts Unreleased"
             (let* ((src "#+title: CHANGELOG\n\n* Unreleased\n\n* 1.1.0 - 2025-11-30\n")
                    (out (l-release--rewrite-changelog src "1.2.0" "2026-05-14")))
               (expect out :to-equal
                       "#+title: CHANGELOG\n\n* Unreleased\n\n* 1.2.0 - 2026-05-14\n\n* 1.1.0 - 2025-11-30\n")))

    (test-it "errors when there is no `* Unreleased' section"
             (expect (l-release--rewrite-changelog "no header" "1.2.0" "2026-05-14")
                     :to-throw))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; release-test.el ends here
