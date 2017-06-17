#!/usr/bin/env cake
;;
;; euler48.clj

;; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
;;
;; Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000
;;
;; Answer = 9110846700

(use '[cbd])
(use '[sage])
(require 'clojure.contrib.math)
(require '[clojure.contrib.combinatorics :only (permutations)])

(defn sum-of-digits-to-digits [n]
  (reduce + (for [i (range 1 (inc n))] (clojure.contrib.math/expt (biginteger i) i))))

(defn euler-48 []
  (bigint (apply str (take-last 10 (str (sum-of-digits-to-digits 1000))))))