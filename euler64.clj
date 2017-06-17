#!/usr/bin/env cake
;;
;; euler64.clj
;;
;;

;; All  square roots are periodic when written as continued fractions and can be written in the form:
;;
;; (n)^1/2 = a0 + 1 / ( a1 + ( 1 / a2 + (1 / a3 ...
;;
;; All square roots repeat. How many continued fractions for N  10000 have an odd period?

;; Iterative Solution for square root of N (per Wikipedia):

;(use 'sage)
(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

(def & comp)
(def p partial)

(def sqrt clojure.math/sqrt)
(def floor clojure.math/floor)

(defn- cf-iterated-expansion-of-sqrt [n]
  "Infinite sequence of the iterated continued fraction expansion of square root of n."
  (iterate
   (fn [[m, d, a ]]
     (let [m (int (- (* d a) m)),
           d (int (/ (- n (* m m)) d))
           a (int (floor (/ (+ (sqrt n) m) d)))]
       [m, d, a]))
   [0, 1, (int (floor (sqrt n)))]))

(defn cf-form-of-sqrt [n]
  "Loop through the iterated continued expansion until the triplet [m, d, a] is repeated."
  (loop [cf (cf-iterated-expansion-of-sqrt n), result []]
;;    (println (take 2 cf) result)
;;    (clojure.java.shell/sh "/bin/sleep" "1")
    (if (some #{(first cf)}  result)
      (split-at 1 (map last result))
      (recur (rest cf), (conj result (first cf))))))

(defn- perfect-square? [n]
  (= (sqrt n) (floor (sqrt n))))

(defn euler-64 [size]
  "Look for all cf-forms-of-sqrt with odd periods, disregarding perfect squares"
  (count
   (for [i (range 1 (inc size))
         :when
         (and (not (perfect-square? i))  ;; Avoid perfect squares
              (odd? (count (second (cf-form-of-sqrt i)))))] i)))

;; (euler-64 10000) -> 1322
