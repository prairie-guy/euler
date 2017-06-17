#!/usr/bin/env cake
;;
;; euler6.clj
;; The sum of the squares of the first ten natural numbers is,
;;
;; 1^2 + 2^2 + ... + 10^2 = 385
;; The square of the sum of the first ten natural numbers is,
;;
;; (1 + 2 + ... + 10)^2 = 552 = 3025
;;
;; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
;;

(require '[cbd])
(use 'clojure.contrib.math)

(defn run1 [num]
  (- (expt (reduce + (range 1 (inc num))) 2) (reduce + (map #(* % %) (range 1 (inc num))))))
