#!/usr/bin/env cake

;; Euler1.clj
;; Add all the natural numbers below one thousand that are multiples of 3 or 5.
;;
;; Answer = 233168

(defn run1 []
  (reduce +
          (filter
           #(or (= 0 (mod % 3)) (= 0 (mod % 5)))
           (range 1000))))

(defn run2 []
  (reduce +
          (for [n (range 1000) :when (or (= 0 (mod n 3)) (= 0 (mod n 5)))] n)))

(defn run3 []
  (->> (range 1000)
       (filter #(or (= 0 (mod % 3)) (= 0 (mod % 5))))
       (reduce +)))
       
