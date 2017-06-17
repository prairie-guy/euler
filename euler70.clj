;;
;; euler70.clj
;;
;;


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

(defn euler70 [size]
  (first
   (sort-by last
            (for [i (range 2 (inc size))
                  :let [d (cbd/integer->digits i),
                        t (cbd/integer->digits (totient i)) ]
                  :when (= (sort d) (sort t))]
              [d t (* 1.0 (/ i (totient i)))])))
  )


;; (euler70 10000000)
;; [(8 3 1 9 8 2 3) (8 3 1 3 9 2 8) 1.000709051124811]  