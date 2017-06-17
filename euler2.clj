#!/usr/bin/env cake

;; Euler2.clj
;; By considering the terms in the Fibonacci sequence whose values do not exceed four million,
;; find the sum of the even-valued terms. 
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fib-iter [num]
  (if (or (= num 0) (= num 1))
    1
    (loop [n 2, f_1 1, f_2 1]
      (if (=  n num)
        (+ f_1 f_2)
        (recur  (inc n) (+ f_1 f_2) f_1)))))


(def fib-iter-seq (map fib-iter (iterate inc 0)))

(defn run1 []
  (reduce + (for [f fib-iter-seq :when (even? f) :while (< f 4000000) ] f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def fib-map-seq (map first (iterate (fn [[n_1 n_2]] [(+ n_1 n_2) n_1]) [1 1])))

(defn run2 []
  (reduce + (take-while (partial >= 4000000) (filter even? fib-map-seq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run3 []
  (->> fib-map-seq
       (filter even?)
       (take-while (partial >= 4000000))
       (reduce +)))
