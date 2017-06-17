#!/usr/bin/env cake
;;
;; euler21.clj

;; Find the sum of the digits in the number 100!
;;

;; Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
;; If d(n) = m and d(m) = n, where n  m, then a and b are an amicable pair and each of a and b are called amicable numbers.
;;
;; i.e., d (d(n)) = n
;;
;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
;; The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
;;
;; Evaluate the sum of all the amicable numbers under 10000.
;;
;; Answer = 31626


(use '[cbd])
; (use 'clojure.contrib.math)

(defn sum-proper-divisors [n]
  (reduce + (butlast (divisors n))))

(def pairs  (for [n (range 10000) :when (let [d (sum-proper-divisors n)] (and (== n (sum-proper-divisors d)) (not (= n d))))]
              (sort [n (sum-proper-divisors n)])))

(defn run[] (reduce + (flatten (distinct pairs))))
  



