#!/usr/bin/env cake
;;
;; euler47.clj
;;
;; The first two consecutive numbers to have two distinct prime factors are:
;; 14 = 2 * 7
;; 15 = 3 * 5
;;
;; The first three consecutive numbers to have three distinct prime factors are:
;;
;; 644 = 2² * 7 * 23
;; 645 = 3 * 5 * 43
;; 646 = 2 * 17 * 19.

;; Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers?

(use '[cbd])
(use '[sage])
(require 'clojure.contrib.math)
(require '[clojure.contrib.combinatorics :only (permutations)])

(defn euler-47 []
  (first (for [p (partition 4 1 (iterate inc 2))
               :when (every? #{4} (map #(count (distinct (factors %))) p))]
           p)))