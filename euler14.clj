#!/usr/bin/env cake
;;
;; euler14.clj
;;
;; The following iterative sequence is defined for the set of positive integers:
;;
;; n ->  n/2 (n is even)
;; n ->  3n + 1 (n is odd)
;; Which starting number, under one million, produces the longest chain?
;;
;; Answer = [837799 525]
;;

(require '[cbd])
(use 'clojure.contrib.math)

(defn collatz-orig [n]
  (loop [m n, result [n]]
    (cond (== m 1) result
          (even? m) (recur (/ m 2) (conj result (/ m 2)))
          :else (recur (inc (* 3 m)) (conj result (inc (* 3 m)))))))

(defn collatz [n]
  (loop [m n, result 1]
    (cond (== m 1) result
          (even? m) (recur (/ m 2) (inc result))
          :else (recur (inc (* 3 m)) (inc result)))))

(defn run[]
  (loop [i 1, len 1, max 1]
    (if (> i 1e6) [max len]
        (let [c (collatz i)]
          (if (<= c len) (recur (inc i) len max)
              (recur (inc i) c i))))))

(defn run-1[]
  (loop [i 1, m {1 1}]
    (cond (> i 1e6) (last (sort-by last m))
          (contains? m i) (recur (inc i) m)
          :else (let [c (collatz i)]
                  (recur (inc i) (assoc m i c))))))
