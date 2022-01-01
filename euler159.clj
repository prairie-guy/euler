;;
;; euler159.clj
;;

(require '[cbd])
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
(require '[clojure.pprint :refer (pprint)])
(require '[clojure.set])
(require '[clojure.data.priority-map :refer (priority-map)])
;(require '[clojure.core.matrix :as mx :refer (pm mget mset!)])
;(require '[clojure.core.matrix.operators :as m ])
;(mx/set-current-implementation :vectorz)
;(mx/set-current-implementation :ndarray)

;; A composite number can be factored many different ways. For instance, not including multiplication by one,
;; 24 can be factored in 7 distinct ways:
;;
;; 24 = 2x2x2x3
;; 24 = 2x3x4
;; 24 = 2x2x6
;; 24 = 4x6
;; 24 = 3x8
;; 24 = 2x12
;; 24 = 24
;;
;; Recall that the digital root of a number, in base 10, is found by adding together the digits of that number,
;; and repeating that process until a number is arrived at that is less than 10. Thus the digital root of 467 is 8.
;;
;; We shall call a Digital Root Sum (DRS) the sum of the digital roots of the individual factors of our number.
;; The chart below demonstrates all of the DRS values for 24.
;;
;; Factorisation       Digital Root Sum
;; 2x2x2x3                     9
;; 2x3x4                       9
;; 2x2x6                      10
;; 4x6                        10
;; 3x8                        11
;; 2x12                        5
;; 24                          6
;;
;; The maximum Digital Root Sum of 24 is 11.
;;
;; The function mdrs (n) gives the maximum Digital Root Sum of n. So mdrs (24)=11.
;; Find ∑mdrs (n) for 1 < n < 1,000,000.


(defn drs [n]
  (if (< n 10) n
      (let [d (mod n 9)]
        (if (== d 0 ) 9
            d))))

(defn divisor-pairs [n]
  "Returns all divisor-pairs for an integer n.
   A divisor-pair is a divisor of n and the complimentary integer, s.t. their product is n.
   [1 n] is not included. If n is prime, nil is returned.
   For example, (divisor-pairs 24) -> ([2 12] [3 8] [4 6])"
  (let [divs-upto-sqrt (filter #(zero? (rem n %)) (range 2 (inc (int (Math/sqrt n)))))]
    (map #(vector % (quot n %)) divs-upto-sqrt)))

(def mdrs
  "A recursive function on n, that finds: (max (drs n), (mdrs dp_0), (mdrs dp_1), (mdrs dp_2) ...)"
  (memoize
   (fn [n]
     (let [dps (divisor-pairs n)]
       (if (nil? dps)
         (drs n)
         (apply max (conj
                     (map #(+ (mdrs (first %)) (mdrs (second  %))) dps)
                     (drs n))))))))

(defn euler159 [max]
  (time (reduce + (for [n (range 2 max)] (mdrs n)))))

;;; (euler159 1e6) -> "Elapsed time: 44685.423 msecs" 14489159


;;; Not actually used in problem. Just an interesting alternative to using the traditional divisor.
(defn divisors [n]
  "Returns all divisors of n, not just prime factors, inclusive of 1 and n.
   Rather than checking all possible factors upto sqrt(n), it finds the prime factors of n, and then
   finds all possible combinations.
   For 360, the prime factors are [2 2 2 3 3]. The divisors are then the cartesian-product of
    [1 2 4 8] [1 3 9] [1 5] NOTE: sort is expensive, so use only if order of divisor matters."
  (let [factor-partitions (map #(conj % 1) (partition-by identity (cbd/factors n)))
        power-series (fn [partition-of-f] (map #(expt %1 %2) partition-of-f (range (count partition-of-f))))]
    (sort (map #(apply * %) (apply combinatorics/cartesian-product
                                   (map power-series factor-partitions))))))
