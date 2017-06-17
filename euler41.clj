#!/usr/bin/env cake
;;
;; euler41.clj
;;
;;

(use '[cbd])
(require 'clojure.contrib.math)
(require '[clojure.contrib.combinatorics :only (permutations)])

;; We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
;; For example, 2143 is a 4-digit pandigital and is also prime.
;; What is the largest n-digit pandigital prime that exists?


(defn largest-pandigital  [digits]
  (last (sort
         (for [d (range 1 (inc (count digits))),
               p (map digits->integer (clojure.contrib.combinatorics/permutations (take d digits)))
               :when (prime? p)] p))))

(defn euler-41 [] (largest-pandigital "123456789"))
