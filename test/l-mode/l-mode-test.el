;;; -*- lexical-binding: t; -*-
;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; l-mode-test.el:
;; Tests for l-mode.el
;;

;;; code:

(require 'l-test-helpers)

(describe "l-mode.el"
  (describe "mode definition"
    (test-it "defines l-mode as a derived mode"
      (expect (fboundp 'l-mode) :to-be t))
    
    (test-it "derives from emacs-lisp-mode"
      (with-temp-buffer
        (l-mode)
        (expect major-mode :to-equal 'l-mode)
        (expect (derived-mode-p 'emacs-lisp-mode) :to-be 'emacs-lisp-mode)))
    
    (test-it "has correct mode name"
      (with-temp-buffer
        (l-mode)
        (expect mode-name :to-equal "L"))))

  (describe "font-lock keywords"
    (test-it "defines additional keywords variable"
      (expect (boundp 'l-mode-additional-keywords) :to-be t)
      (expect l-mode-additional-keywords :to-be-truthy)
      (expect (length l-mode-additional-keywords) :to-equal 4))
    
    (test-it "adds keywords to font-lock when mode is activated"
      (with-temp-buffer
        (l-mode)
        (expect font-lock-keywords :to-be-truthy)
        ;; Check that our keywords are present
        (let ((keywords-found (cl-some (lambda (keyword)
                                         (and (listp keyword)
                                              (stringp (car keyword))
                                              (string-match-p "@doc" (car keyword))))
                                       font-lock-keywords)))
          (expect keywords-found :to-be-truthy)))))

  (describe "@doc highlighting"
    (before-each
      (setq test-buffer (generate-new-buffer "*l-mode-test*"))
      (with-current-buffer test-buffer
        (l-mode)))
    
    (after-each
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))
    
    (test-it "highlights @doc keyword"
      (with-current-buffer test-buffer
        (insert "@doc \"This is documentation\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "@doc")
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (expect face :to-equal 'font-lock-constant-face))))
    
    (test-it "highlights @doc string"
      (with-current-buffer test-buffer
        (insert "@doc \"This is documentation\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "\"This is documentation\"")
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (expect face :to-equal 'font-lock-doc-face))))
    
    (test-it "highlights multiline @doc strings"
      (with-current-buffer test-buffer
        (insert "@doc \"This is\na multiline\ndocumentation\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "@doc")
        (let ((doc-face (get-text-property (match-beginning 0) 'face)))
          (expect doc-face :to-equal 'font-lock-constant-face))
        (search-forward "multiline")
        (let ((string-face (get-text-property (match-beginning 0) 'face)))
          (expect string-face :to-equal 'font-lock-doc-face))))
    
    (test-it "handles @doc with whitespace"
      (with-current-buffer test-buffer
        (insert "@doc    \"Documentation with spaces\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "@doc")
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (expect face :to-equal 'font-lock-constant-face))))
    
    (test-it "handles @doc with tabs"
      (with-current-buffer test-buffer
        (insert "@doc\t\"Documentation with tab\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "@doc")
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (expect face :to-equal 'font-lock-constant-face)))))

  (describe "backtick highlighting in @doc strings"
    (before-each
      (setq test-buffer (generate-new-buffer "*l-mode-test*"))
      (with-current-buffer test-buffer
        (l-mode)))
    
    (after-each
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))
    
    (test-it "highlights backtick expressions in @doc strings"
      (with-current-buffer test-buffer
        (insert "@doc \"This function uses `variable-name' for processing\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "variable-name")
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (expect face :to-contain 'font-lock-constant-face))))
    
    (test-it "highlights multiple backtick expressions"
      (with-current-buffer test-buffer
        (insert "@doc \"Uses `var1' and `var2' parameters\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "var1")
        (let ((face1 (get-text-property (match-beginning 0) 'face)))
          (expect face1 :to-contain 'font-lock-constant-face))
        (search-forward "var2")
        (let ((face2 (get-text-property (match-beginning 0) 'face)))
          (expect face2 :to-contain 'font-lock-constant-face))))
    
    (test-it "handles backticks in multiline @doc strings"
      (with-current-buffer test-buffer
        (insert "@doc \"This is a multiline\ndocumentation with `backticks'\nacross lines\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "backticks")
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (expect face :to-contain 'font-lock-constant-face))))       
    
    (test-it "handles escaped quotes in @doc strings with backticks"
      (with-current-buffer test-buffer
        (insert "@doc \"This has \\\"escaped quotes\\\" and `backticks'\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "backticks")
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (expect face :to-contain 'font-lock-constant-face)))))  

  (describe "edge cases"
    (before-each
      (setq test-buffer (generate-new-buffer "*l-mode-test*"))
      (with-current-buffer test-buffer
        (l-mode)))
    
    (after-each
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))
    
    (test-it "handles @doc at beginning of buffer"
      (with-current-buffer test-buffer
        (insert "@doc \"First line documentation\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (let ((face (get-text-property (point) 'face)))
          (expect face :to-equal 'font-lock-constant-face))))
    
    (test-it "handles @doc at end of buffer"
      (with-current-buffer test-buffer
        (insert "some code\n@doc \"Last line documentation\"")
        (font-lock-ensure)
        (goto-char (point-max))
        (backward-word)
        (let ((face (get-text-property (point) 'face)))
          (expect face :to-equal 'font-lock-doc-face))))
    
    (test-it "handles empty @doc strings"
      (with-current-buffer test-buffer
        (insert "@doc \"\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "@doc")
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (expect face :to-equal 'font-lock-constant-face))))
    
    (test-it "handles @doc with only backticks"
      (with-current-buffer test-buffer
        (insert "@doc \"`function-name'\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "function-name")
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (expect face :to-contain 'font-lock-constant-face))))
    
    (test-it "handles nested quotes in @doc strings"
      (with-current-buffer test-buffer
        (insert "@doc \"This is a \\\"quoted\\\" string\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "quoted")
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (expect face :to-contain 'font-lock-doc-face))))
    
    (test-it "handles @doc followed by code"
      (with-current-buffer test-buffer
        (insert "@doc \"Documentation\"\n(ldef test-func (x) (+ x 1))")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "Documentation")
        (let ((doc-face (get-text-property (match-beginning 0) 'face)))
          (expect doc-face :to-equal 'font-lock-doc-face))
        (search-forward "ldef")
        (let ((ldef-face (get-text-property (match-beginning 0) 'face)))
          (expect ldef-face :to-equal 'font-lock-keyword-face))))
    
    (test-it "handles @doc in comments"
      (with-current-buffer test-buffer
        (insert ";; @doc \"This is in a comment\"")
        (font-lock-ensure)
        (goto-char (point-min))
        (let ((face (get-text-property (point) 'face)))
          (expect face :to-equal 'font-lock-comment-delimiter-face))))
    
    (test-it "handles malformed @doc (missing quotes)"
      (with-current-buffer test-buffer
        (insert "@doc missing quotes")
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "@doc")
        ;; Should still highlight @doc even if malformed
        (let ((face (get-text-property (match-beginning 0) 'face)))
          (expect face :not :to-equal 'font-lock-constant-face))))))
