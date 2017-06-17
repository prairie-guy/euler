#!/usr/bin/env cake
;;
;; euler58.clj
;;
;;

;; Starting with 1 and spiralling anticlockwise in the following way, a square
;; spiral with side length 7 is formed.

;; 37 36 35 34 33 32 31
;; 38 17 16 15 14 13 30
;; 39 18  5  4  3 12 29
;; 40 19  6  1  2 11 28
;; 41 20  7  8  9 10 27
;; 42 21 22 23 24 25 26
;; 43 44 45 46 47 48 49

;; It is interesting to note that the odd squares lie along the bottom
;; right diagonal, but what is more interesting is that 8 out of the 13
;; numbers lying along both diagonals are prime; that is, a ratio of 8/13  62%.

;; If one complete new layer is wrapped around the spiral above, a square spiral
;; with side length 9 will be formed. If this process is continued, what is the
;; side length of the square spiral for which the ratio of primes along both
;; diagonals first falls below 10%?

;(use 'sage)
(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

;; From problem 28
(defn diagonals-in-square-orig [size]
  (assert (and (> size 0) (odd? size)))
  (loop [side (dec size), ith (* size size),  result []]
    (if (zero? side) (concat  result [1])
        (recur (- side 2),
               (- ith (* 4 side)),
               (concat result (for [n (range ith (- ith (* 4 side)) (- side))] n))))))

;; Cleaner version of problem 28
(defn corners [n]
  (assert (and (> n 1) (odd? n)))
  [(* n n), (+ (* n n) (- n) 1 ), (+ (* n n) (- (* 2 n)) 2 ), (+ (* n n) (- (* 3 n)) 3 ) ])

(defn diagonals-in-square [size]
  (assert (and (> size 0) (odd? size)))
  (last
   (first
    (filter #(= (first %) size)
            (iterate (fn [[n, diags]] [(+ n 2), (concat diags (corners (+ n 2)))]) [1, [1]])))))

;; Problem 58
(defn one-if-prime [n] (if (cbd/prime? n) 1 0))
(defn count-primes [ns] (apply + (map one-if-prime ns)))
(def third #(nth % 2))

(defn euler58 []
  (first
   (filter #(< (/ (third %) (second %)) 10/100 )
           (iterate (fn [[n, num-cs, num-ps]] [(+ n 2),
                                               (+ num-cs 4),
                                               (+ num-ps (count-primes (corners (+ n 2))))])
                    [3,5,3]))))
