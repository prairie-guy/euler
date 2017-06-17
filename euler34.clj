#!/usr/bin/env cake
;;
;; euler34.clj
;;
;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
;;
;; Find the sum of all numbers which are equal to the sum of the factorial of their digits.
;;
;; Note: as 1! = 1 and 2! = 2 are not sums they are not include
;;
;; (145 40585)
;; Answer = 40730
;; 

(use '[cbd])
(use 'clojure.contrib.math)

(defn euler34 []
  (let [fact (apply hash-map (interleave (range 10) (map fact (range 10))))]
    (reduce + (for [n (range 3 1000000) :when (= n (reduce +  (map fact (integer->list n))))] n))))


        
                                                          
    




                