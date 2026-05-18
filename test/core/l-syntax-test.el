;;; -*- lexical-binding: t; -*-
;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; l-syntax-test.el:
;; Tests for l-syntax.el
;;

;;; code:

(require 'l-test-helpers)
(require 'l-main)
(require 'l-syntax)

(describe "l-syntax.el"
  (before-all
(ldef add a b  -> (+ a b))
    (l-syntax-advices))
  (after-all
    (l-syntax-remove-advices))
  (describe "l-syntax-advices"
    (before-each
      ;; Mock advice-add to track calls
      (setq l-syntax-advice-calls '())
      (advice-add 'advice-add :around
                  (lambda (orig-fun symbol where function &optional props)
                    (push (list symbol where function props) l-syntax-advice-calls)
                    ;; Don't actually add the advice during testing
                    nil)))
  
    (after-each
      ;; Clean up mock
      (advice-remove 'advice-add
                     (lambda (orig-fun symbol where function &optional props)
                       (push (list symbol where function props) l-syntax-advice-calls)
                       nil)))
  
    (test-it "adds advice to all evaluation functions"
             (l-syntax-advices)
             (expect (length l-syntax-advice-calls) :to-equal 5)
             (expect (assoc 'eval-last-sexp l-syntax-advice-calls) :to-be-truthy)
             (expect (assoc 'eval-region l-syntax-advice-calls) :to-be-truthy)
             (expect (assoc 'eval-buffer l-syntax-advice-calls) :to-be-truthy)
             (expect (assoc 'load-file l-syntax-advice-calls) :to-be-truthy)
             (expect (assoc 'load l-syntax-advice-calls) :to-be-truthy))
  
    (test-it "uses :around advice for all functions"
             (l-syntax-advices)
             (dolist (call l-syntax-advice-calls)
               (expect (nth 1 call) :to-equal :around))))

  (describe "l-syntax-remove-advices"
    (before-each
      ;; Mock advice-remove to track calls
      (setq l-syntax-remove-calls '())
      (advice-add 'advice-remove :around
                  (lambda (orig-fun symbol function)
                    (push (list symbol function) l-syntax-remove-calls)
                    ;; Don't actually remove the advice during testing
                    nil)))
  
    (after-each
      ;; Clean up mock
      (advice-remove 'advice-remove
                     (lambda (orig-fun symbol function)
                       (push (list symbol function) l-syntax-remove-calls)
                       nil)))
  
    (test-it "removes advice from all evaluation functions"
             (l-syntax-remove-advices)
             (expect (length l-syntax-remove-calls) :to-equal 5)
             (expect (assoc 'eval-last-sexp l-syntax-remove-calls) :to-be-truthy)
             (expect (assoc 'eval-region l-syntax-remove-calls) :to-be-truthy)
             (expect (assoc 'eval-buffer l-syntax-remove-calls) :to-be-truthy)
             (expect (assoc 'load-file l-syntax-remove-calls) :to-be-truthy)
             (expect (assoc 'load l-syntax-remove-calls) :to-be-truthy)))
  (describe "l--eval-last-sexp-advice"
    (test-it "throws error when l-syntax is nil"
             ;; Set eval-expression-debug-on-error to nil to prevent Emacs 30's
             ;; handler-bind from invoking the debugger, which would interrupt
             ;; the test before buttercup can catch the error with :to-throw
             (let ((l-syntax nil)
                   (eval-expression-debug-on-error nil))
               (with-temp-buffer
                 (insert "((add 1) 2)")
                 (goto-char (point-max))
                 (expect (eval-last-sexp nil) :to-throw))))
    
    (test-it "works when l-syntax is nil but prop variable is set"
             (let ((l-syntax nil))
               (with-temp-buffer
                 (insert ";; -*- l-syntax: t; -*-\n((add 1) 2)")
                 (goto-char (point-max))
                 (expect (eval-last-sexp nil) :to-equal 3))))
    
    (test-it "works when l-syntax is bound to t in let"
             (let ((l-syntax t))
               (with-temp-buffer
                 (insert "((add 1) 2)")
                 (goto-char (point-max))
                 (expect (eval-last-sexp nil) :to-equal 3)))))
  (describe "l--eval-region-advice"
    (test-it "throws error when l-syntax is nil"
             (let ((l-syntax nil))
               (with-temp-buffer
                 (insert "((add 1) 2)")
                 (mark-whole-buffer)
                 (expect (eval-region (point-min) (point-max)) :to-throw))))
    
    (test-it "works when l-syntax is nil but prop variable is set"
             (let ((l-syntax nil)
                   (test-result nil))
               (with-temp-buffer
                 (insert ";; -*- l-syntax: t; -*-\n(setq test-result ((add 1) 2))")
                 (mark-whole-buffer)
                 (expect (eval-region (point-min) (point-max)) :to-equal 3))))
    
    (test-it "works when l-syntax is bound to t in let"
             (let ((l-syntax t)
                   (test-result nil))
               (with-temp-buffer
                 (insert "((add 1) 2)")
                 (mark-whole-buffer)
                 (expect (eval-region (point-min) (point-max)) :to-equal 3)))))
  (describe "l--eval-buffer-advice"
    (test-it "throws error when l-syntax is nil"
             (let ((l-syntax nil))
               (with-temp-buffer
                 (insert "((add 1) 2)")
                 (expect (funcall-interactively 'eval-buffer) :to-throw))))
    
    (test-it "works when l-syntax is nil but prop variable is set"
             (let ((l-syntax nil)
                   (test-result nil))
               (with-temp-buffer
                 (insert ";; -*- l-syntax: t; -*-\n((add 1) 2)")
                 (expect (funcall-interactively 'eval-buffer) :to-equal 3))))
    
    (test-it "works when l-syntax is bound to t in let"
             (let ((l-syntax t)
                   (test-result nil))
               (with-temp-buffer
                 (insert "((add 1) 2)")
                 (expect (funcall-interactively 'eval-buffer) :to-equal 3)))))
  
  (describe "l--load-advice"
    (let ((temp-file-path nil)
          (temp-file-with-prop nil))
      
      (before-each
        ;; Create temporary files for testing
        (setq temp-file-path (make-temp-file "l-syntax-test" nil ".el"))
        (setq temp-file-with-prop (make-temp-file "l-syntax-test-prop" nil ".el"))
        
        ;; Write content to temp files
        (with-temp-file temp-file-path
          (insert "(setq load-test-result ((add 1) 2))"))
        
        (with-temp-file temp-file-with-prop
          (insert ";; -*- l-syntax: t; -*-\n(setq load-test-result-prop ((add 1) 2))")))
      
      (after-each
        ;; Clean up temp files
        (when (file-exists-p temp-file-path)
          (delete-file temp-file-path))
        (when (file-exists-p temp-file-with-prop)
          (delete-file temp-file-with-prop))
        ;; Clean up test variables
        (when (boundp 'load-test-result)
          (makunbound 'load-test-result))
        (when (boundp 'load-test-result-prop)
          (makunbound 'load-test-result-prop)))
      
      (test-it "throws error when l-syntax is nil"
               (let ((l-syntax nil))
                 (expect (load temp-file-path) :to-throw)))
      
      (test-it "works when l-syntax is nil but prop variable is set"
               (let ((l-syntax nil))
                 (load temp-file-with-prop)
                 (expect load-test-result-prop :to-equal 3)))
      
      (test-it "works when l-syntax is bound to t in let"
               (let ((l-syntax t))
                 (load temp-file-path)
                 (expect load-test-result :to-equal 3)))))
  (describe "l--load-file-advice"
    (let ((temp-file-path nil)
          (temp-file-with-prop nil))
      
      (before-each
        ;; Create temporary files for testing
        (setq temp-file-path (make-temp-file "l-syntax-test" nil ".el"))
        (setq temp-file-with-prop (make-temp-file "l-syntax-test-prop" nil ".el"))
        
        ;; Write content to temp files
        (with-temp-file temp-file-path
          (insert "(setq load-file-test-result ((add 1) 2))"))
        
        (with-temp-file temp-file-with-prop
          (insert ";; -*- l-syntax: t; -*-\n(setq load-file-test-result-prop ((add 1) 2))")))
      
      (after-each
        ;; Clean up temp files
        (when (file-exists-p temp-file-path)
          (delete-file temp-file-path))
        (when (file-exists-p temp-file-with-prop)
          (delete-file temp-file-with-prop))
        ;; Clean up test variables
        (when (boundp 'load-file-test-result)
          (makunbound 'load-file-test-result))
        (when (boundp 'load-file-test-result-prop)
          (makunbound 'load-file-test-result-prop)))
      
      (test-it "throws error when l-syntax is nil"
               (let ((l-syntax nil))
                 (expect (load-file temp-file-path) :to-throw)))
      
      (test-it "works when l-syntax is nil but prop variable is set"
               (let ((l-syntax nil))
                 (load-file temp-file-with-prop)
                 (expect load-file-test-result-prop :to-equal 3)))
      
      (test-it "works when l-syntax is bound to t in let"
               (let ((l-syntax t))
                 (load-file temp-file-path)
                 (expect load-file-test-result :to-equal 3))))
    (describe "@doc"
      (describe "when using `eval-buffer'"
        (test-it "throws error when l-syntax is nil"
                 (let ((l-syntax nil))
                   (with-temp-buffer
                     (insert "@doc \"Adds two numbers\"\n(ldef add-doc a b -> (+ a b))")
                     (goto-char (point-max))
                     (expect (eval-buffer nil) :to-throw))))
        
        (test-it "works when l-syntax is nil but prop variable is set"
                 (let ((l-syntax nil))
                   (with-temp-buffer
                     (insert ";; -*- l-syntax: t; -*-\n@doc \"Adds two numbers\"\n(ldef add-doc a b -> (+ a b))")
                     (goto-char (point-max))
                     (eval-buffer nil)
                     (expect (string-match-p "Adds two numbers"
                                             (documentation 'add-doc))))))
        
        (test-it "works when l-syntax is bound to t in let"
                 (let ((l-syntax t))
                   (with-temp-buffer
                     (insert "@doc \"Adds two numbers\"\n(ldef add-doc a b -> (+ a b))")
                     (goto-char (point-max))
                     (eval-buffer nil)
                     (expect (string-match-p "Adds two numbers"
                                             (documentation 'add-doc)))))))
      (describe "@doc"
        (describe "when using `eval-last-sexp'"
          (test-it "throws error when l-syntax is nil"
                   ;; Set eval-expression-debug-on-error to nil to prevent Emacs 30's
                   ;; handler-bind from invoking the debugger, which would interrupt
                   ;; the test before buttercup can catch the error with :to-throw
                   (let ((l-syntax nil)
                         (eval-expression-debug-on-error nil))
                     (with-temp-buffer
                       (insert "(progn\n@doc \"Adds two numbers\"\n(ldef add-doc-last a b -> (+ a b)))")
                       (goto-char (point-max))
                       (expect (eval-last-sexp nil) :to-throw))))
          
          (test-it "works when l-syntax is nil but prop variable is set"
                   (let ((l-syntax nil))
                     (with-temp-buffer
                       (insert ";; -*- l-syntax: t; -*-\n(progn\n@doc \"Adds two numbers\"\n(ldef add-doc-last a b -> (+ a b)))")
                       (goto-char (point-max))
                       (eval-last-sexp nil)
                       (expect (string-match-p "Adds two numbers"
                                               (documentation 'add-doc-last))))))
          
          (test-it "works when l-syntax is bound to t in let"
                   (let ((l-syntax t))
                     (with-temp-buffer
                       (insert "(progn\n@doc \"Adds two numbers\"\n(ldef add-doc-last a b -> (+ a b)))")
                       (goto-char (point-max))
                       (eval-last-sexp nil)
                       (expect (string-match-p "Adds two numbers"
                                               (documentation 'add-doc-last)))))))
        
        (describe "when using `eval-region'"
          (test-it "throws error when l-syntax is nil"
                   (let ((l-syntax nil))
                     (with-temp-buffer
                       (insert "@doc \"Adds two numbers\"\n(ldef add-doc-region a b -> (+ a b))")
                       (mark-whole-buffer)
                       (expect (eval-region (point-min) (point-max)) :to-throw))))
          
          (test-it "works when l-syntax is nil but prop variable is set"
                   (let ((l-syntax nil))
                     (with-temp-buffer
                       (insert ";; -*- l-syntax: t; -*-\n@doc \"Adds two numbers\"\n(ldef add-doc-region a b -> (+ a b))")
                       (mark-whole-buffer)
                       (eval-region (point-min) (point-max))
                       (expect (string-match-p "Adds two numbers"
                                               (documentation 'add-doc-region))))))
          
          (test-it "works when l-syntax is bound to t in let"
                   (let ((l-syntax t))
                     (with-temp-buffer
                       (insert "@doc \"Adds two numbers\"\n(ldef add-doc-region a b -> (+ a b))")
                       (mark-whole-buffer)
                       (eval-region (point-min) (point-max))
                       (expect (string-match-p "Adds two numbers"
                                               (documentation 'add-doc-region)))))))
        
        (describe "when using `load'"
          (let ((temp-file-path nil)
                (temp-file-with-prop nil))
            
            (before-each
              ;; Create temporary files for testing
              (setq temp-file-path (make-temp-file "l-syntax-doc-test" nil ".el"))
              (setq temp-file-with-prop (make-temp-file "l-syntax-doc-test-prop" nil ".el"))
              
              ;; Write content to temp files
              (with-temp-file temp-file-path
                (insert "@doc \"Adds two numbers\"\n(ldef add-doc-load a b -> (+ a b))"))
              
              (with-temp-file temp-file-with-prop
                (insert ";; -*- l-syntax: t; -*-\n@doc \"Adds two numbers\"\n(ldef add-doc-load-prop a b -> (+ a b))")))
            
            (after-each
              ;; Clean up temp files
              (when (file-exists-p temp-file-path)
                (delete-file temp-file-path))
              (when (file-exists-p temp-file-with-prop)
                (delete-file temp-file-with-prop))
              ;; Clean up function definitions
              (when (fboundp 'add-doc-load)
                (fmakunbound 'add-doc-load))
              (when (fboundp 'add-doc-load-prop)
                (fmakunbound 'add-doc-load-prop)))
            
            (test-it "throws error when l-syntax is nil"
                     (let ((l-syntax nil))
                       (expect (load temp-file-path) :to-throw)))
            
            (test-it "works when l-syntax is nil but prop variable is set"
                     (let ((l-syntax nil))
                       (load temp-file-with-prop)
                       (expect (string-match-p "Adds two numbers"
                                               (documentation 'add-doc-load-prop)))))
            
            (test-it "works when l-syntax is bound to t in let"
                     (let ((l-syntax t))
                       (load temp-file-path)
                       (expect (string-match-p "Adds two numbers"
                                               (documentation 'add-doc-load)))))))
        
        (describe "when using `load-file'"
          (let ((temp-file-path nil)
                (temp-file-with-prop nil))
            
            (before-each
              ;; Create temporary files for testing
              (setq temp-file-path (make-temp-file "l-syntax-doc-file-test" nil ".el"))
              (setq temp-file-with-prop (make-temp-file "l-syntax-doc-file-test-prop" nil ".el"))
              
              ;; Write content to temp files
              (with-temp-file temp-file-path
                (insert "@doc \"Adds two numbers\"\n(ldef add-doc-load-file a b -> (+ a b))"))
              
              (with-temp-file temp-file-with-prop
                (insert ";; -*- l-syntax: t; -*-\n@doc \"Adds two numbers\"\n(ldef add-doc-load-file-prop a b -> (+ a b))")))
            
            (after-each
              ;; Clean up temp files
              (when (file-exists-p temp-file-path)
                (delete-file temp-file-path))
              (when (file-exists-p temp-file-with-prop)
                (delete-file temp-file-with-prop))
              ;; Clean up function definitions
              (when (fboundp 'add-doc-load-file)
                (fmakunbound 'add-doc-load-file))
              (when (fboundp 'add-doc-load-file-prop)
                (fmakunbound 'add-doc-load-file-prop)))
            
            (test-it "throws error when l-syntax is nil"
                     (let ((l-syntax nil))
                       (expect (load-file temp-file-path) :to-throw)))
            
            (test-it "works when l-syntax is nil but prop variable is set"
                     (let ((l-syntax nil))
                       (load-file temp-file-with-prop)
                       (expect (string-match-p "Adds two numbers"
                                               (documentation 'add-doc-load-file-prop)))))
            
            (test-it "works when l-syntax is bound to t in let"
                     (let ((l-syntax t))
                       (load-file temp-file-path)
                       (expect (string-match-p "Adds two numbers"
                                               (documentation 'add-doc-load-file)))))))
        
        (describe "when using `eval-buffer'"
          (test-it "throws error when l-syntax is nil"
                   (let ((l-syntax nil))
                     (with-temp-buffer
                       (insert "@doc \"Adds two numbers\"\n(ldef add-doc a b -> (+ a b))")
                       (goto-char (point-max))
                       (expect (eval-buffer nil) :to-throw))))
          
          (test-it "works when l-syntax is nil but prop variable is set"
                   (let ((l-syntax nil))
                     (with-temp-buffer
                       (insert ";; -*- l-syntax: t; -*-\n@doc \"Adds two numbers\"\n(ldef add-doc a b -> (+ a b))")
                       (goto-char (point-max))
                       (eval-buffer nil)
                       (expect (string-match-p "Adds two numbers"
                                               (documentation 'add-doc))))))
          
          (test-it "works when l-syntax is bound to t in let"
                   (let ((l-syntax t))
                     (with-temp-buffer
                       (insert "@doc \"Adds two numbers\"\n(ldef add-doc a b -> (+ a b))")
                       (goto-char (point-max))
                       (eval-buffer nil)
                       (expect (string-match-p "Adds two numbers"
                                               (documentation 'add-doc)))))))))))

(describe "l-syntax.el load-pipeline integration"
  (before-all
    (ldef add a b -> (+ a b))
    (l-syntax-advices))
  (after-all
    (l-syntax-remove-advices))

  (describe "load-history"
    (test-it "records the loaded file"
      (let ((temp-file (make-temp-file "l-history-test" nil ".el"))
            (l-syntax t))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (insert "(defvar l-history-test-var ((add 1) 2))\n"))
              (load temp-file)
              (expect (cl-some (lambda (entry)
                                 (and (stringp (car entry))
                                      (string-suffix-p
                                       (file-name-nondirectory temp-file)
                                       (car entry))))
                               load-history)
                      :to-be-truthy))
          (delete-file temp-file)
          (when (boundp 'l-history-test-var)
            (makunbound 'l-history-test-var))))))

  (describe "load-file-name"
    (test-it "is bound during file evaluation"
      (let ((temp-file (make-temp-file "l-lfn-test" nil ".el"))
            (l-syntax t))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (insert "(setq l-lfn-captured load-file-name)\n"))
              (load temp-file)
              (expect (boundp 'l-lfn-captured) :to-be-truthy)
              (expect l-lfn-captured :to-be-truthy)
              (expect (string-suffix-p ".el" l-lfn-captured) :to-be-truthy))
          (delete-file temp-file)
          (when (boundp 'l-lfn-captured)
            (makunbound 'l-lfn-captured))))))

  (describe "features tracking during load"
    (test-it "(require X) inside an l-syntax-loaded file sees provides from features"
      ;; Exercises the load pipeline that the old `l-syntax--load' bypass
      ;; broke: `provide' must update `features' early enough that a sibling
      ;; `(require X)' in the same chain is a no-op rather than re-loading
      ;; the file.  Files A and B mutually `require' each other; both
      ;; `provide' first, so the second `require' in each finds the feature
      ;; already in `features' and returns immediately.
      (let* ((temp-dir (file-name-as-directory
                        (make-temp-file "l-features-test" t)))
             (a-file (expand-file-name "l-features-a.el" temp-dir))
             (b-file (expand-file-name "l-features-b.el" temp-dir))
             (load-path (cons temp-dir load-path))
             (l-syntax t))
        (unwind-protect
            (progn
              (with-temp-file a-file
                (insert "(provide 'l-features-a)\n"
                        "(require 'l-features-b)\n"))
              (with-temp-file b-file
                (insert "(provide 'l-features-b)\n"
                        "(require 'l-features-a)\n"))
              (expect (require 'l-features-a) :to-equal 'l-features-a)
              (expect (featurep 'l-features-a) :to-be-truthy)
              (expect (featurep 'l-features-b) :to-be-truthy))
          (delete-directory temp-dir t)
          (setq features (delq 'l-features-a features))
          (setq features (delq 'l-features-b features))))))

  (describe "after-load-functions"
    (test-it "fires for l-syntax-loaded files"
      (let* ((temp-file (make-temp-file "l-after-load-test" nil ".el"))
             (l-syntax t)
             (fired-file nil)
             (hook (lambda (f) (setq fired-file f))))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (insert "(defvar l-after-load-marker t)\n"))
              (add-hook 'after-load-functions hook)
              (load temp-file)
              (expect fired-file :to-be-truthy)
              (expect (string-suffix-p (file-name-nondirectory temp-file)
                                       fired-file)
                      :to-be-truthy))
          (remove-hook 'after-load-functions hook)
          (delete-file temp-file)
          (when (boundp 'l-after-load-marker)
            (makunbound 'l-after-load-marker))))))

  (describe "@doc grouping with intervening form"
    (test-it "groups each @doc with the next form, leaving others untouched"
      (let ((temp-file (make-temp-file "l-doc-intervene-test" nil ".el"))
            (l-syntax t))
        (unwind-protect
            (progn
              (with-temp-file temp-file
                (insert "@doc \"First doc\"\n"
                        "(ldef l-intervene-first a -> a)\n"
                        "(setq l-intervene-marker 'middle)\n"
                        "@doc \"Second doc\"\n"
                        "(ldef l-intervene-second b -> b)\n"))
              (load temp-file)
              (expect (string-match-p "First doc"
                                      (documentation 'l-intervene-first))
                      :to-be-truthy)
              (expect l-intervene-marker :to-equal 'middle)
              (expect (string-match-p "Second doc"
                                      (documentation 'l-intervene-second))
                      :to-be-truthy))
          (delete-file temp-file)
          (when (boundp 'l-intervene-marker)
            (makunbound 'l-intervene-marker))
          (when (fboundp 'l-intervene-first)
            (fmakunbound 'l-intervene-first))
          (when (fboundp 'l-intervene-second)
            (fmakunbound 'l-intervene-second)))))))
