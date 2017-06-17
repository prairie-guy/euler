#!/usr/bin/env cake
;;
;; euler30.clj
;;
;; Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
;;
;; 1634 = 1^4 + 6^4 + 3^4 + 4^4
;; 8208 = 8^4 + 2^4 + 0^4 + 8^4
;; 9474 = 9^4 + 4^4 + 7^4 + 4^4
;; As 1 = 1^4 is not a sum it is not included.
;;
;; The sum of these numbers is 1634 + 8208 + 9474 = 19316.
;;
;; Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
;;
;; Answer = 443839

(use '[cbd])
(use 'clojure.contrib.math)

(defn euler30 []
  (let [fifths (zipmap (range 10) (map #(Math/pow % 5) (range 10)))
        sum-fifth? (fn [n] (==  n (reduce + (map #(fifths %) (integer->list n)))))]
    (reduce + (for [i (range 10 (expt 10 6)) :when (sum-fifth? i)] i))))
