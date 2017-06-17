;;
;; euler71.clj
;;
;;


;; Consider the fraction, n/d, where n and d are positive integers. If nd and HCF (n,d)=1,
;; it is called a reduced proper fraction. If we list the set of reduced proper fractions
;; for d  8 in ascending order of size, we get:
;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;; It can be seen that 2/5 is the fraction immediately to the left of 3/7.

;; By listing the set of reduced proper fractions for d  1,000,000 in ascending order
;; of size, find the numerator of the fraction immediately to the left of 3/7.

;(use 'sage)
(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

(def sqrt clojure.math/sqrt)
(def floor clojure.math/floor)

(defn farey-seq [n]
  (concat
   (map #(take 2 %)
        (take-while #(<= (nth % 2) n )
                    (iterate
                     (fn [[a, b, c, d]]
                       (let [k (int (/ (+ n b) d))]
                         [c, d, (- (* k c) a), (- (* k d) b)]))
                     [0,1,1,n])))
   '((1 1))))

(defn fract-form [fs]
  (map #(/ (first %) (second %)) fs))

;; Really inefficient
;; (defn euler71 [nd, n]
;;   ;; nd should be a fractional represenation '(n d)
;;   (last (take-while #(not= % nd) (farey-seq n))))



(defn euler71 [N]
  (let [mediant? (fn [l r n] (= (inc n) (+ (second l) (second r))))
        mediant  (fn [l r]   [(+ (first l) (first r)) (+ (second l) (second r))])]
    (last
     (take (- N 6)
           (iterate
            (fn [[l, m, r, n]]
              (cond
               (and (mediant? l m n) (mediant? m r n))[(mediant l m) m (mediant m r) (inc n)]
               (mediant? l m n) [(mediant l m) m r (inc n)]
               (mediant? m r n) [l m (mediant m r) (inc n)]
               :else [l m r (inc n)]))
            [[2 5] [3 7] [1 2 ] 7 ])))))

;; (euler71 1000000) -> [[428570 999997] [3 7] [428569 999994] 1000000]
;; 428570


