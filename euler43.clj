#!/usr/bin/env cake
;;
;; euler43.clj
;;

(use '[cbd])
(require 'clojure.contrib.math)
(require 'clojure.string)
(require '[clojure.contrib.combinatorics :only (permutations)])   

;; The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.
;; Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

;; d2d3d4=406 is divisible by 2
;; d3d4d5=063 is divisible by 3
;; d4d5d6=635 is divisible by 5
;; d5d6d7=357 is divisible by 7
;; d6d7d8=572 is divisible by 11
;; d7d8d9=728 is divisible by 13
;; d8d9d10=289 is divisible by 17
;;
;; Find the sum of all 0 to 9 pandigital numbers with this property.
;;
;; Answer = 16695334890N

;;(def p (clojure.contrib.combinatorics/permutations (range 0 10)))
(def pan-digitals (clojure.contrib.combinatorics/permutations "0123456789"))
(defn to-i [s] (digits->integer s))

(defn divisibility? [s]
  (every? zero? 
          (vector 
           (rem (to-i (subs s 1 4)) 2)
           (rem (to-i (subs s 2 5)) 3)
           (rem (to-i (subs s 3 6)) 5)
           (rem (to-i (subs s 4 7)) 7)
           (rem (to-i (subs s 5 8)) 11)
           (rem (to-i (subs s 6 9)) 13)
           (rem (to-i (subs s 7 10)) 17))))
  
(defn euler-43 []
  (reduce +
          (map bigint
               (filter divisibility? (map #(apply str %) pan-digitals)))))
   