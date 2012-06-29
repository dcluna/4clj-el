(require 'json)

(defun 4clj-get-problem (number)
  (url-retrieve 
   (concat "http://www.4clojure.com/api/problem/" (number-to-string number))))

(defun 4clj-parse-problem (json-problem)
  json-problem)

(provide 'four-clj)