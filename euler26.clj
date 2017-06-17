#!/usr/bin/env cake
;;
;; euler26.clj
;;
(use '[cbd])
(use 'clojure.contrib.math)

(defn justify [d p]
  (let [shift-by (fn [d] (int (Math/ceil (- (Math/log10 d) 1))))]
    (with-precision p :rounding FLOOR (* (expt 10 (shift-by d)) (/ 1M d)))))

(defn testing [d times]
  (doseq [s (range 1 times)] (prn (with-precision (* 2 s) :rounding FLOOR (* (justify d (* 2 s)) (expt 10M s))))))

(defn shift [d times]
  (loop [s 1]
    (let [num (with-precision (* 2 s) :rounding FLOOR (* (justify d (* 2 s)) (expt 10M s)))
          int-part (bigint num)
          dec-part (- num int-part)]
      (cond (>= s times) ["*", d, s]
            (zero? (- int-part (* dec-part (expt 10M s)))) [d, s int-part]
            :else  (recur (inc s))))))

;;; Actual Code is here

(defn order-10-mod-p [prime]
  (inc (count (take-while #(not= % (biginteger 1)) (map #(.modPow (biginteger 10)  (biginteger %) (biginteger prime)) (iterate inc 1))))))

(defn run [size]
      (last (sort #(< (second %1) (second %2))  (for [d (range 3 size) :when (and (not= 0 (mod d 5)) (odd? d))] [d (order-10-mod-p d)]))))
;;      (reduce (fn[x y] (if (> (second x) (second y)) x y))  (for [d (range 3 size) :when (and (not= 0 (mod d 5)) (odd? d))] [d (order-10-mod-p d)])))
