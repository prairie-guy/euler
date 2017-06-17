#!/usr/bin/env cake
;;
;; euler62.clj
;;
;;


;(use 'sage)
(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

(def & comp)
(def p partial)

(defn cube [n]
  (* n n n))

(defn cubes-map-up-to [x]
  (for [i (range x) ]
    {:i i, :i_3 (cube i), :sorted_digits (sort (cbd/integer->digits (cube i))) } ))

(defn euler62 [i-max n-perms]
  (first (sort (flatten
                (for [[k vs] (group-by :sorted_digits (cubes-map-up-to i-max)) :when (= n-perms (count vs))]
                  (map :i_3 vs))))))

;; (euler62 10000 5) -> 127035954683
