#!/usr/bin/env cake
;;
;; euler52.clj
;;


;; It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.


;(use 'sage)
(use 'clojure.repl)
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set]) 

(def expt clojure.math/expt)
(def join clojure.string/join)
(def trim clojure.string/trim)

(defn digit-map [n]
  (frequencies (cbd/digits n)))

(defn euler-52 [max]
  (loop [x 1]
    (cond (> x max) "Nothing found"
          (apply = (map digit-map [x (* 2 x) (* 3 x) (* 4 x) (* 5 x) (* 6 x)])) [x (* 2 x) (* 3 x) (* 4 x) (* 5 x) (* 6 x)]
          :else (recur  (inc x)))))

