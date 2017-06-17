;;
;; euler77.clj
;;

(require 'cbd)
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
;; (require '[clojure.set])


;; It is possible to write ten as the sum of primes in exactly five different ways:

;; 7 + 3
;; 5 + 5
;; 5 + 3 + 2
;; 3 + 3 + 2 + 2
;; 2 + 2 + 2 + 2 + 2

;; What is the first value which can be written as the sum of primes in over
;; five thousand different ways?

;; K(n) :: Prime Partitions for n
;; K(0) = 1; K(1) = 0; K(2) = 1
;; K(n) = (1/n) [sopf(n) + Sum[j=1..n-1, (sopf(j) K(n - j)
;;
;;

(def sopf
  "Sum of the unique primes of n"
  (memoize (fn [n] (reduce + (distinct (cbd/factors n))))))

(def k
  "Prime Partitions of n"
  (memoize (fn [n]
             (cond (= n 0), 1
                   (= n 1), 0
                   :else
                   (* (/ 1 n)
                      (+ (sopf n)
                         (reduce + (for [j, (range 1 n) ] (* (sopf j) (k (- n j)))))))))))

(def K
  (memoize
   (fn [n]
     (if (= n 0) 1
         (* (/ 1 n)
            (+ (sopf n)
               (reduce + (map #(* (sopf %) (K (- n %)))  (range 1, n )))))))))

(defn K_large [n]
  (last (map K (range (inc n)))))

(defn euler77 []
  (some #(if (> (K %) 5000) %) (range)))


;; (euler77) -> [71 5007N]
