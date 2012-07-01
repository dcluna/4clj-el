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
  '4clj-parse-problem
  "The default callback function for use with 4clj-get-problem.")

(defvar 4clj-comment-string ";;"
  "The comment marker.")

(defvar 4clj-template-function '4clj-default-template
  "The template function.")

;;; Code:

(defun 4clj-default-template (tests)
  "Returns the template as a string."
  (concat "(let [__ \"Solution here\"]\n" tests "\n )"))

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
           ;;(signal (car status-data) (cdr status-data))
           (message "Error fetching problem"))
          ((equal :redirect status-type)
           ;;(error (concat "Redirected to " status-data))
           (message "Problem does not exist")))))

(defun 4clj-parse-problem (status)
  "Returns a parsed 4clojure problem fetched from URL-BUFFER in 4clj-current-problem."
  (next-line 8)
  (beginning-of-line)
  (kill-line)
  (if (null status)
      (setq 4clj-current-problem (json-read-from-string (car kill-ring)))
    (4clj-fetch-error status)))

(defun 4clj-insert-problem-template (test-vector)
  "Inserts the TESTS inside `4clj-problem-template` and puts them in the current buffer."
  ;; (let ((tests (mapconcat (lambda (str) str) test-vector "\n")))
   ; (insert )
  ))

(defun make-4clj-buffer (name)
  "Makes a buffer called NAME populated with a 4clojure problem."
  (set-buffer (get-buffer-create name))
  (let ((problem-description (cdr (assoc 'description 4clj-current-problem)))
        (problem-tests (cdr (assoc 'tests 4clj-current-problem))))
    (insert (concat 4clj-comment-string problem-description))
    (let* ((test-vector (cdr (assoc 'tests 4clj-current-problem)))
           (tests (mapconcat (lambda (str) str) test-vector "\n")))
      (insert (4clj-template-function tests)))
    (if (featurep 'clojure-mode)
        (clojure-mode))))

(provide 'four-clj)

;;; four-clj.el ends here
