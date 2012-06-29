;;; four-clj.el --- 4clojure's minor mode for emacs

;;; Commentary:
;; This is a minor-mode for interaction with 4clojure. It fetches a
;; problem from the site and puts it into the buffer
;; `4clj-problem-buffer-name`, with a template for resolution.

(require 'json)

(defvar 4clj-current-problem nil
  "Holds the current 4clojure problem.")

;;; Code:

(defun 4clj-get-problem (number callback)
  "Retrieves the NUMBER 4clojure problem for use with CALLBACK."
  (url-retrieve
   (concat "http://www.4clojure.com/api/problem/" (number-to-string number)) callback))

(defun 4clj-fetch-error (status)
  "Signals the error type in STATUS."
  (let ((status-type (car status))
        (status-data (cdr status)))
    (cond ((equal :error status-type)
           (signal (car status-data) (cdr status-data)))
          ((equal :redirect status-type)
           (error (concat "Redirected to " status-data))))))

(defun 4clj-get-json-string (status)
  "Returns the 4clojure problem JSON fetched from URL-BUFFER."
  (next-line 8)
  (beginning-of-line)
  (kill-line)
  (if (null status)
      (setq 4clj-current-problem (4clj-parse-problem (car kill-ring)))
    (4clj-fetch-error status)))

;; (4clj-get-problem 1 '4clj-get-json-string)

;; (4clj-parse-problem (concat "{" 4clj-current-problem "}"))

;; (4clj-parse-problem 4clj-current-problem)

(defun 4clj-parse-problem (json-problem)
  (json-read-from-string json-problem))

(provide 'four-clj)

;;; four-clj.el ends here
