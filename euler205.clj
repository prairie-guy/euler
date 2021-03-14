;;
;; euler205.clj
;;
;; Peter has nine four-sided (pyramidal) dice, each with
;; faces numbered 1, 2, 3, 4.
;; Colin has six six-sided (cubic) dice, each with
;; faces numbered 1, 2, 3, 4, 5, 6.
;;
;; Peter and Colin roll their dice and compare totals:
;; the highest total wins. The result is a draw if the totals are equal.
;;
;; What is the probability that Pyramidal Pete beats Cubic Colin?
;; Give your answer rounded to seven decimal places
;; in the form 0.abcdefg
;;

(require '[cbd])
(require '[clojure.math.numeric-tower :as cmath])
(require '[clojure.math.combinatorics :as comb])
(use '[clojure.string :only (join trim split split-lines)])
(require '[clojure.set])
(use 'clojure.repl)

;; (dice-sums [1 2 3] [1 2 3]) ->
;; {2 1, 3 2, 4 3, 5 2, 6 1}
(defn dice-sums [& lsts]
  (into (sorted-map)
        (frequencies
         (map #(reduce + %) (apply comb/cartesian-product (vec lsts))))))

;; (dice-probs (dice-sums [1 2 3] [1 2 3])) ->
;; {2 1/9, 3 2/9, 4 1/3, 5 2/9, 6 1/9}
(defn dice-probs [dice-sums]
  (let [sum (reduce + (vals dice-sums))]
    (into (sorted-map)
          (for [[x y] dice-sums] [x (/ y sum)]))))

;; A and B are dice-probs
;; i.e., {2 1/9, 3 2/9, 4 1/3, 5 2/9, 6 1/9}
;; Returns P(A beating B)
(defn winning-probs [A B]
  (reduce +
          (for [[sa pa] A
                [sb pb] B
                :when (> sa sb)]
            (* 1.0 pa pb))))

(def pyramidal (dice-probs
                (dice-sums [1 2 3 4][1 2 3 4][1 2 3 4][1 2 3 4][1 2 3 4]
                           [1 2 3 4][1 2 3 4][1 2 3 4][1 2 3 4])))

(def cubic (dice-probs
            (dice-sums [1 2 3 4 5 6][1 2 3 4 5 6][1 2 3 4 5 6]
                       [1 2 3 4 5 6][1 2 3 4 5 6][1 2 3 4 5 6])))

(defn euler208 []
  (/ (cmath/round (* 10000000
                    (winning-probs pyramidal cubic)))
    10000000.0))

;; user> (time (euler208))
;; "Elapsed time: 1.86721 msecs"
;; => 0.5731441
