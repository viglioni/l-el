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

  (describe "s-expressions and backslashes in @doc strings"
    (before-each
      (setq test-buffer (generate-new-buffer "*l-mode-test*"))
      (with-current-buffer test-buffer
        (l-mode)))
    
    (after-each
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))
    
    (describe "s-expression highlighting"
      (test-it "highlights simple s-expressions"
               (with-current-buffer test-buffer
                 (insert "@doc \"Use (+ 1 2) for addition\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "(+ 1 2)")
                 (let ((face (get-text-property (match-beginning 0) 'face)))
                   (expect face :to-contain 'font-lock-variable-name-face))))
      
      (test-it "highlights nested s-expressions"
               (with-current-buffer test-buffer
                 (insert "@doc \"Complex example: (lcomp 'inc (l x -> (* 2 x)) (l x -> (* 3 x))) 10\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "(lcomp 'inc (l x -> (* 2 x)) (l x -> (* 3 x)))")
                 (let ((face (get-text-property (match-beginning 0) 'face)))
                   (expect face :to-contain 'font-lock-variable-name-face))))
      
      (test-it "highlights multiple s-expressions in same docstring"
               (with-current-buffer test-buffer
                 (insert "@doc \"First (+ 1 2) and second (* 3 4) expressions\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "(+ 1 2)")
                 (let ((face1 (get-text-property (match-beginning 0) 'face)))
                   (expect face1 :to-contain 'font-lock-variable-name-face))
                 (search-forward "(* 3 4)")
                 (let ((face2 (get-text-property (match-beginning 0) 'face)))
                   (expect face2 :to-contain 'font-lock-variable-name-face))))
      
      (test-it "highlights s-expressions with strings inside"
               (with-current-buffer test-buffer
                 (insert "@doc \"Example: (message \\\"hello world\\\")\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "(message \\\"hello world\\\")")
                 (let ((face (get-text-property (match-beginning 0) 'face)))
                   (expect face :to-contain 'font-lock-variable-name-face))))
      
      (test-it "highlights deeply nested s-expressions"
               (with-current-buffer test-buffer
                 (insert "@doc \"Deep nesting: (outer (middle (inner 42)))\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "(outer (middle (inner 42)))")
                 (let ((face (get-text-property (match-beginning 0) 'face)))
                   (expect face :to-contain 'font-lock-variable-name-face))))
      
      (test-it "highlights s-expressions in multiline docstrings"
               (with-current-buffer test-buffer
                 (insert "@doc \"This function uses\n(complex-calculation x y)\nfor processing\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "(complex-calculation x y)")
                 (let ((face (get-text-property (match-beginning 0) 'face)))
                   (expect face :to-contain 'font-lock-variable-name-face))))
      
      (test-it "handles malformed s-expressions gracefully"
               (with-current-buffer test-buffer
                 (insert "@doc \"Incomplete expression: (+ 1 2 and valid (- 5 3)\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "(- 5 3)")
                 (let ((face (get-text-property (match-beginning 0) 'face)))
                   (expect face :to-contain 'font-lock-variable-name-face))))
      
      (test-it "doesn't highlight s-expressions outside @doc strings"
               (with-current-buffer test-buffer
                 (insert "(+ 1 2)\n@doc \"Inside doc (- 3 4)\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "(+ 1 2)")
                 (let ((face1 (get-text-property (match-beginning 0) 'face)))
                   (expect face1 :not :to-contain 'font-lock-variable-name-face))
                 (search-forward "(- 3 4)")
                 (let ((face2 (get-text-property (match-beginning 0) 'face)))
                   (expect face2 :to-contain 'font-lock-variable-name-face)))))
    
    (describe "backslash highlighting"
      (test-it "highlights single backslashes"
               (with-current-buffer test-buffer
                 (insert "@doc \"Use \\\\n for newlines\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "\\\\")
                 (let ((face (get-text-property (match-beginning 0) 'face)))
                   (expect face :to-contain 'shadow))))
      
      (test-it "highlights backslashes in escaped quotes"
               (with-current-buffer test-buffer
                 (insert "@doc \"This has \\\\\\\"escaped quotes\\\\\\\"\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "\\\\\\\"")
                 (let ((face (get-text-property (match-beginning 0) 'face)))
                   (expect face :to-contain 'shadow))))
      
      (test-it "highlights multiple backslashes"
               (with-current-buffer test-buffer
                 (insert "@doc \"Multiple escapes: \\\\n \\\\t \\\\r\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "\\\\n")
                 (let ((face1 (get-text-property (match-beginning 0) 'face)))
                   (expect face1 :to-contain 'shadow))
                 (search-forward "\\\\t")
                 (let ((face2 (get-text-property (match-beginning 0) 'face)))
                   (expect face2 :to-contain 'shadow))))
      
      (test-it "highlights backslashes in multiline docstrings"
               (with-current-buffer test-buffer
                 (insert "@doc \"Line one with \\\\n\nLine two with \\\\t\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "\\\\n")
                 (let ((face1 (get-text-property (match-beginning 0) 'face)))
                   (expect face1 :to-contain 'shadow))
                 (search-forward "\\\\t")
                 (let ((face2 (get-text-property (match-beginning 0) 'face)))
                   (expect face2 :to-contain 'shadow))))
      
      (test-it "highlights backslashes within s-expressions"
               (with-current-buffer test-buffer
                 (insert "@doc \"Example: (message \\\"hello\\\\nworld\\\")\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "\\\\n")
                 (let ((face (get-text-property (match-beginning 0) 'face)))
                   (expect face :to-contain 'shadow))))           
      
      (test-it "handles consecutive backslashes"
               (with-current-buffer test-buffer
                 (insert "@doc \"Multiple backslashes: \\\\\\\\\\\\\\\\\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "\\\\\\\\")
                 (let ((face (get-text-property (match-beginning 0) 'face)))
                   (expect face :to-contain 'shadow)))))
    
    (describe "combined s-expressions and backslashes"
      (test-it "highlights both in complex examples"
               (with-current-buffer test-buffer
                 (insert "@doc \"Function (format \\\"Value: %s\\\\n\\\" x) formats output\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "(format \\\"Value: %s\\\\n\\\" x)")
                 (let ((sexp-face (get-text-property (match-beginning 0) 'face)))
                   (expect sexp-face :to-contain 'font-lock-variable-name-face))
                 (goto-char (point-min))
                 (search-forward "\\\\n")
                 (let ((backslash-face (get-text-property (match-beginning 0) 'face)))
                   (expect backslash-face :to-contain 'shadow))))
      
      (test-it "handles nested s-expressions with escaped content"
               (with-current-buffer test-buffer
                 (insert "@doc \"Complex: (lcomp (l x -> (message \\\"Processing %s\\\\n\\\" x)) list)\"")
                 (font-lock-ensure)
                 (goto-char (point-min))
                 (search-forward "(lcomp (l x -> (message \\\"Processing %s\\\\n\\\" x)) list)")
                 (let ((sexp-face (get-text-property (match-beginning 0) 'face)))
                   (expect sexp-face :to-contain 'font-lock-variable-name-face))
                 (goto-char (point-min))
                 (search-forward "\\\\n")
                 (let ((backslash-face (get-text-property (match-beginning 0) 'face)))
                   (expect backslash-face :to-contain 'shadow))))))

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
