#!/usr/bin/env cake
;;
;; euler44.clj
;;

(use '[cbd])
(require 'clojure.contrib.math)
(require 'clojure.string)
(require '[clojure.contrib.combinatorics :only (permutations)])   

;; Pentagonal numbers are generated by the formula, Pn=n(3n-1)/2. The first ten pentagonal numbers are:
;; 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
;; It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference, 70  22 = 48, is not pentagonal.
;; Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference is pentagonal and
;; D = |Pk  Pj| is minimised; what is the value of D?

(defn nth-pent [indx] (/ (* indx (- (* 3 indx) 1)) 2))
;;(defn pent? [n] (if (some #(= % n) (take-while #(<= % n) pent-numbs)) true false))

(defn pent? [n] (zero? (rem (+ 1 (Math/sqrt (+ 1 (* 24 n)))) 6)))

(defn euler-44 []
  (first (for [p pent-numbs, q (take-while #(< % p) pent-numbs):when (pent? (abs (- p q))) :when  (pent? (+ p q))] [p q (- p q)])))

