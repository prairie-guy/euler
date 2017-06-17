#!/usr/bin/env cake
;;
;; euler46.clj
;;
;; It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
;; 
;; 9  = 7  + 2 * 1^2
;; 15 = 7  + 2 * 2^2
;; 21 = 3  + 2 * 3^2
;; 25 = 7  + 2 * 3^2
;; 27 = 19 + 2 * 2^2
;; 33 = 31 + 2 * 1^2
;;
;; It turns out that the conjecture was false.
;;
;; What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?


(use '[cbd])
(use '[sage])
(require 'clojure.contrib.math)
(require '[clojure.contrib.combinatorics :only (permutations)])
    
(def composites (filter #(not (prime? %)) (iterate inc 1)))

(def two-sqrs (map first (iterate (fn [[n ith]] [(* 2 ith ith) (inc ith)]) [1 1])))

(defn goldbach? [c]
  (first (for [p (take-while #(<= % c) primes), sq (take-while #(<= (+ p %) c) two-sqrs) :when (= c (+ p sq))] (+ p sq))))

(defn euler-46 []
  (first (for [n (filter odd? composites) :when (not (goldbach? n))] n)))

