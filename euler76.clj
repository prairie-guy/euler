;;
;; euler76.clj
;;
(use 'tupelo.core)
(use 'cbd)
(use '[clojure.string :only (join trim split split-lines)])
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(def sqrt clojure.math/sqrt)
(def floor clojure.math/floor)

;; It is possible to write five as a sum in exactly six different ways:

;; 4 + 1
;; 3 + 2
;; 3 + 1 + 1
;; 2 + 2 + 1
;; 2 + 1 + 1 + 1
;; 1 + 1 + 1 + 1 + 1

;; How many different ways can 100  be written as a sum of at least two positive integers?

(defn sigma_1 [n]
  "The divisor function sigma_k(n) for n=1 is defined as the sum of
 the (positive integer) divisors of n"
  (if (= n 1)
    1
    (reduce + (divisors n))))

(defn- partitions-k [n ps]
  "k-th iteration of partition-n
  ps(k) is a vector memoizing previous paritions,
  ps = [1,1,2,3,5...]. (ps k) -> kth value of ps"
  (* (/ 1 n)
     (summation 0 (dec n)
                (fn [k] (* (sigma_1 (- n k)) (ps k))))))

(defn partitions-n [n]
  "Calculate the partion itegers of n"
  (it-> (iterate
         (fn [[k ps]] ; ps is an array of previous partitionsy
           [(inc k) (append ps (partitions-k k ps))])
         [1 [1]])
        (nth it n)
        (last (last it))))



(defn euler76 []
  (dec (partitions-fn 100)))

;; (euler76 100) -> 190569292

