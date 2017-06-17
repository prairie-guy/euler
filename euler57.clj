#!/usr/bin/env cake
;;
;; euler57.clj
;;
;;

;; It is possible to show that the square root of two can be expressed as an infinite continued fraction.
;; 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
;;
;; By expanding this for the first four iterations, we get:
;;
;; 1 + 1/2 = 3/2 = 1.5
;; 1 + 1/(2 + 1/2) = 7/5 = 1.4
;; 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
;; 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
;;
;; The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the
;; first example where the number of digits in the numerator exceeds the number of digits in the denominator.
;;
;; In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?


;(use 'sage)
(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

(def & comp)
(def p partial)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage: (extend-pf 0)             -> '(/ 1 (+ 2 ))
;;        (extend-pd (extend-pf 0)) -> '(/ 1 (+ 2 (/ 1 (+ 2 0))))

(defn extend-pf [p] (eval (list '/ 1 (list '+ 2 p))))

(defn continued-fraction [n]
  (rest (take (inc n) (map inc (iterate extend-pf 0)))))

(defn euler-57 []
  (count
   (for [rat (continued-fraction 1000)
        :when (> (cbd/integer-count (numerator rat)) (cbd/integer-count (denominator rat)))]
     rat)))
