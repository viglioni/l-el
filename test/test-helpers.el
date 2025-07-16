;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; test-helpers.el:
;; Set up buttercup and some macro helpers.
;;

;;; code:

(require 'l-load-path)

;;;###autoload
(defun load-test-file (file-name)
  "Search for FILE-NAME and load it."
  (mapcar 'load
          (directory-files-recursively "." (concat file-name ".el"))))

;;;###autoload
(defmacro context (description &rest body)
  "The same as `describe', but more idiomatic and with defun indentation.
DESCRIPTION is the context description: string.
BODY is the test block body."
  (declare (indent defun))
  `(describe (concat "when " ,description)
             ,@body))

;;;###autoload
(defmacro test-it (description &rest body)
  "The same as `it', but with defun indentation.
DESCRIPTION is the test suit title and
BODY is the test suit block."
  (declare (indent defun))
  `(it (concat "it " ,description)
       ,@body))

(provide 'test-helpers)

;;; test-helpers.el ends here.
