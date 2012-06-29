(load (concat default-directory "four-clj.el"))

;; (ert-deftest 4clj-test-get-problem ()
;;   "Tests fetching of 4clojure problems"
;;   )

(ert-deftest 4clj-parse-problem ()
  "Tests parsing of 4clojure's JSON problems"
  (let ((json-problem "{\"description\":\"When operating on a Vector, the conj function will return a new vector with one or more items \"added\" to the end.\",\"difficulty\":\"Elementary\",\"restricted\":[],\"scores\":{\"10\":4,\"12\":112,\"14\":2,\"18\":2,\"20\":2,\"29\":1,\"6\":728,\"7\":23,\"9\":5},\"tags\":[],\"tests\":[\"(= __ (conj [1 2 3] 4))\",\"(= __ (conj [1 2] 3 4))\"],\"times-solved\":1165,\"title\":\"Vectors: conj\",\"user\":\"dbyrne\"}")
        (result '()))
    (should (= (4clj-parse-problem json-problem)
               result))) )