#!/usr/bin/env cake
;;
;; euler49.clj
;;
;; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
;; (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
;;
;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
;; 
;; What 12-digit number do you form by concatenating the three terms in this sequence?

(use '[cbd])
(use '[sage])
(require 'clojure.contrib.math)
(require '[clojure.contrib.combinatorics :only (permutations)])

(defn euler-49 []
  (let [f-d-p  (filter #(= (count (str %)) 4) (take-while #(<= (count (str %)) 4) primes))]
    (for [f f-d-p
          :when (let [g (+ f 3330),
                      h (+ f 6660)
                      perms (map #(Integer. (apply str %))  (clojure.contrib.combinatorics/permutations (digits f)))]
                  (and (some #{g} f-d-p)
                       (some #{h} f-d-p)
                       (some #{g} perms)
                       (some #{h} perms)))]
      (apply str [f (+ f 3330) (+ f 6660)]))))