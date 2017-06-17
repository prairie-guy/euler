;;
;; euler69.clj
;;
;;

;; Euler's Totient function, φ (n) [sometimes called the phi function], is used to determine the
;; number of numbers less than n which are relatively prime to n. For example, as
;; 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ (9)=6.
;; Find the value of n  1,000,000 for which n/φ (n) is a maximum.

;(use 'sage)
(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

(def sqrt clojure.math/sqrt)
(def floor clojure.math/floor)

(defn totient [n]
  (* n
     (reduce * 1
             (map #(- 1 (/ 1 %)) (distinct (cbd/factors n))))))

(defn euler69 [size]
  (reverse
   (last
    (sort
     (for [n (range 1 (inc size))] [(/ n (totient n)) n ])))))


