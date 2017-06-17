#!/usr/bin/env cake
;;
;; euler32.clj
;;

(use '[cbd])
(use 'clojure.contrib.math)


;; We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
;; For example, the 5-digit number, 15234, is 1 through 5 pandigital.
;;
;; 7254 is unusual in that 39 * 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
;;
;; Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
;;
;; Answer = 45228

(defn prod-pandigit? [x y]
  (= (range 1 10)
    (sort(concat (integer->list x) (integer->list y)  (integer->list (* x y))))))

(defn euler32 []
  (reduce + (distinct (concat
                       (for [x (range 1 9999), y (range 1 10) :when (prod-pandigit? x y)] (* x y))
                       (for [x (range 1 999), y (range 1 100) :when (prod-pandigit? x y)] (* x y))))))

