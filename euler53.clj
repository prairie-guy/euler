#!/usr/bin/env cake
;;
;; euler52.clj
;;

;;There are exactly ten ways of selecting three from five, 12345:
;;123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
;; It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
;; How many, not necessarily distinct, values of  nCr, for 1<= n <=  100, are greater than one-million?


;(use 'sage)
(use 'clojure.repl)
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set]) 

(def expt clojure.math/expt)
(def join clojure.string/join)
(def trim clojure.string/trim)

(defn euler-53 [max, above]
  (count (filter #(>= % above)
          (for [n (range  (inc max)), k (range  (inc n))]
            (cbd/comb n k)))))
