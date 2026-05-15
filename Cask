(source gnu)
(source melpa)

(package "l.el" "1.1.2" "Modern functional programming utilities and syntax.")

(depends-on "emacs" "29")
(depends-on "cl-lib")

(development
 (depends-on "buttercup")
 ;; org-make-toc now requires Emacs >= 28.2, which breaks `cask install` on
 ;; the 26.3/27.2 CI matrix entries. It is only used as a `before-save-hook'
 ;; in readme.org, so it's safe to omit from CI. Uncomment locally if needed.
 ;; (depends-on "org-make-toc")
 )
