;;
;; euler73.clj
;;
;;

;; Consider the fraction, n/d, where n and d are positive integers. If nd and HCF (n,d)=1,
;; it is called a reduced proper fraction. If we list the set of reduced proper fractions
;; for d  8 in ascending order of size, we get:
;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

;; It can be seen that there are 3 fractions between 1/3 and 1/2.
;; How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper
;; fractions for d <= 12,000?


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

(defn len-farey-seq [n]
  ;; Includes the end point 
  (+ 1 (reduce + (for [i (range 1 (inc n))] (totient i)))))

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

(defn farey-range [n m]
  (doseq [i (range (dec n) m) ] (println (farey-seq i))))

(defn take-through-farey-seq [ab n]
  ;; ab is vector of form [a b]
  (take-while #(not= % ab) (farey-seq n)))


(defn euler73 [n]
  (dec (- (/ (dec (len-farey-seq n)) 2) (count (take-through-farey-seq [1 3] n)))))
