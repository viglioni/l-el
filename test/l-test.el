;;; -*- lexical-binding: t; -*-
;;
;; @author Laura Viglioni
;; 2026
;;
;; GNU Public License 3.0
;;
;; since: 1.1.2
;;
;; l-test.el:
;; Smoke test for the umbrella `l.el' entry point.
;; Intentionally requires only `l' (no submodules) so that any
;; breakage in l.el's top-level loading is caught by CI.
;;

;;; code:

(require 'l)

(describe "l.el umbrella entry point"
  (it "loads and exposes the current version"
    (expect (l-version) :to-equal "1.1.2")))

;;; l-test.el ends here.
