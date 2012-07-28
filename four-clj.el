;;; four-clj.el --- 4clojure's minor mode for emacs

;;; Commentary:
;; This is a minor-mode for interaction with 4clojure. It fetches a
;; problem from the site and puts it into the buffer
;; `4clj-problem-buffer-name`, with a template for resolution.

(require 'url)
(require 'json)

(defvar 4clj-current-problem nil
  "Holds the current 4clojure problem.")

(defconst 4clj-default-callback
  '4clj-parse-and-display
  "The default callback function for use with 4clj-get-problem.")

(defvar 4clj-comment-string ";;"
  "The comment marker.")

(defvar 4clj-template-function '4clj-default-template
  "The template function.")

(defvar 4clj-buffer-name "4clojure"
  "Buffer name prefix.")

;;; Code:

(defun 4clj-default-template (tests)
  "Returns the template as a string."
  (concat "(let [__ \"Solution here\"]\n  (every? true?\n    [" tests "]\n ))"))

(defun 4clj-get-problem (number &optional callback)
  "Retrieves the NUMBER 4clojure problem for use with CALLBACK."
  (url-retrieve
   (concat "http://www.4clojure.com/api/problem/" (number-to-string number)) 
   (if callback
       callback
     4clj-default-callback)))

(defun 4clj-fetch-error (status)
  "Signals the error type in STATUS."
  (let ((status-type (car status))
        (status-data (cdr status)))
    (cond ((equal :error status-type)
           (message "Error fetching problem"))
          ((equal :redirect status-type)
           (message "Problem does not exist")))))

(defun 4clj-parse-problem (status)
  "Returns a parsed 4clojure problem fetched from URL-BUFFER in 4clj-current-problem."
  (next-line 8)
  (beginning-of-line)
  (if (null status)
      (let ((json-problem (buffer-substring (point) (point-max))))
        (setq 4clj-current-problem (json-read-from-string json-problem)))
    (4clj-fetch-error status)))

(defun make-4clj-buffer (name)
  "Makes a buffer called NAME populated with a 4clojure problem."
  (set-buffer (get-buffer-create name))
  (erase-buffer)
  (let ((problem-description (cdr (assoc 'description 4clj-current-problem)))
        (problem-tests (cdr (assoc 'tests 4clj-current-problem))))
    (insert (concat 4clj-comment-string problem-description "\n\n"))
    (let* ((test-vector (cdr (assoc 'tests 4clj-current-problem)))
           (tests (mapconcat (lambda (str) str) test-vector "\n")))
      (insert (funcall 4clj-template-function tests)))))

(defun adjust-4clj-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-region (point-min) (point-max) nil)
    (while (replace-regexp "" ""))))

(defun 4clj-parse-and-display (status)
  (4clj-parse-problem status)
  (if (null status)
      (let ((buffer-name 4clj-buffer-name))
        (make-4clj-buffer buffer-name)
        (switch-to-buffer-other-window buffer-name)
        (if (featurep 'clojure-mode)
            (clojure-mode))
      (adjust-4clj-buffer))        
    (message "Something went terribly wrong.")))

(defun 4clojure-problem (number)
  (interactive "nProblem number:")
  (4clj-get-problem number))

(provide 'four-clj)

;;; four-clj.el ends here
