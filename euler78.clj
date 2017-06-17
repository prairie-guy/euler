;;
;; euler78.clj
;;

(require 'cbd)
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
;;; (require '[clojure.core.memoize])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
;; (require '[clojure.set])


;; Let P (n) represent the number of different ways in which n coins can be
;; separated into piles. For example, five coins can separated into piles in
;; exactly seven different ways, so P (5)=7.
;; Find the least value of n for which P (n) is divisible by one million.

;; Eulers Formula
;; P(n) :: Partitions for n

;; P(n) = 0 for n < 0
;; P(0) = 1
;; P(n) = P(n-1) + P(n-2) - P(n-5) - P(n-7) + P(n-12) + P(n-15) - ...
;;            ^        ^        ^        ^         ^         ^
;;    ^ = {1, 2, 5, 7, 12, 15, ...}
;; Pentagon [k] = k(3k-1)/2 for k = {1, −1, 2, −2, 3, -3, ...}
;;
;; P(n) = {1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77, ...}


(defn pentagon [n]
  (/ (* n (dec (* 3 n))) 2))

(def all-pentagons (drop 2 (map pentagon (interleave (range) (map - (range))))))

(def P
  (memoize
   (fn [n]
    (cond
     (< n 0), 0
     (= n 0), 1N
     :else
     (reduce +
             (map * (map #(P (- n %)) (take-while #(>= n %) all-pentagons))
                    (cycle [1 1 -1 -1])))))))

(defn euler78 [div]
  (some #(if (= 0 (mod (P %) div)) %) (range)))


;; (time (euler78 [1000000]) --> 55374 "ElaPsed time: 27854.343 msecs"
