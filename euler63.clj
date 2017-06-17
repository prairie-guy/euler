#!/usr/bin/env cake
;;
;; euler63.clj
;;
;;

;; The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number,
;; 134217728=8^9, is a ninth power.
;; How many n-digit positive integers exist which are also an nth power?


;(use 'sage)
(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

(def & comp)
(def p partial)

(defn digits-eq-power? [base pow]
  (= pow (cbd/integer-count (clojure.math/expt base pow))))

(defn digits-eq-power-by-power [pow range-max]
  (map #(clojure.math/expt % pow) (filter #(digits-eq-power? % pow) (range range-max))))

(defn euler63 [base-max]
  (dec                    ;; Remove 0
   (count (flatten
           (map #(digits-eq-power-by-power % 500) (range base-max))))))

; (euler63 50) -> 49