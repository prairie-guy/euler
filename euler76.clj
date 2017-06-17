;;
;; euler76.clj
;;

(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

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

(defn divisors-1 [n]
  (if (= n 1) 1
      (reduce + (cbd/divisors n))))

(defn- partitions-helper [n, ps]
  ;; ps is a vector of all paritions[n] n<- {0,n-1}, p[0]=1, p[1]=1 and ps = [1,1,2,3,5...]
  (* (/ 1 n)
     (cbd/sum 0 (dec n) (fn [k] (* (divisors-1 (- n k)) (ps k))))))

(defn partition-fn [n]
  (last (last
         (nth
          (iterate (fn [[m,ps]] [(inc m), (conj ps (partitions-helper m ps))])
                   [1 [1]])
          n))))


(defn euler76 []
  (dec (partition-fn 100)))

;; (euler76 100) -> 190569292
