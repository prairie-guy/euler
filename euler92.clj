;;
;; euler92.clj
;;

(require '[cbd])
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
(require '[clojure.pprint :refer (pprint)])
;;(require '[clojure.set])
;;(require '[clojure.data.priority-map :refer (priority-map)])
;;(require '[clojure.core.matrix :as mx :refer (pm mget mset!)])
;;(require '[clojure.core.matrix.operators :as m ])
;;(mx/set-current-implementation :vectorz)
;;(mx/set-current-implementation :ndarray)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A number chain is created by continuously adding the square of the digits in a number to form a new
;; number until it has been seen before.
;;
;; For example,
;;
;; 44 → 32 → 13 → 10 → 1 → 1
;; 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
;;
;; Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing
;; is that EVERY starting number will eventually arrive at 1 or 89.
;;
;; How many starting numbers below ten million will arrive at 89?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square-digit-sum [n]
  (reduce + (map #(* % %) (cbd/integer->digits n))))

(defn square-digit-chain [n]
  (iterate square-digit-sum n))

(defn attractor [n]
  "Straight-forward implementation"
  (first (filter #(or (= % 1) (= % 89)) (square-digit-chain n))))



(def known (atom {1 1, 89 89}))
(defn attractor-1 [n]
  "Memoization approach. Work through the chain until a known element is found. Elements before are labled 1 or 89.
   Doesn't really improve performance too much. I assume that this is because the chains are reasonably short
   before a 1 or 89 appear. Also, much more overhead."
  (loop [chain (square-digit-chain n), unknown []]
    (let [[key value] (find @known (first chain))]
     (cond
      value (do
           (doseq [i unknown] (swap! known assoc i value))
           value)
      :else (recur (rest chain), (conj unknown (first chain)))))))


(def cache (into {} (for [i (range 1 568)] [i (attractor i)])))
(defn attractor-2 [n]
  "For any number less than 10,000,000, the square-digit-sum must be less than 7*9^2 = 567.
   Precompute square-digit-sum for all digits below 567 and then lookup the result for all larger."
  (if (<= n 568)
    (cache n)
    (cache (square-digit-sum n))))


(defn euler92 []
  (time
   (count
    (for [n (range 1 1e7)
          :when (= 89 (attractor-2 n))]
      n))))


;; Using attractor  : (euler92) -> "Elapsed time: 63782.30 msecs" 8581146
;; Using attractor-1: (euler92) -> "Elapsed time: 37386.15 msecs" 8581146
;; Using attractor-2: (euler92) -> "Elapsed time: 17525.39 msecs" 8581146
