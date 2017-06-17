#!/usr/bin/env cake
;;
;; euler40.clj
;;
;;

(use '[cbd])
(require 'clojure.contrib.math)
(use '[sage])

;; An irrational decimal fraction is created by concatenating the positive integers:
;; 0.123456789101112131415161718192021...
;;
;; It can be seen that the 12th digit of the fractional part is 1.
;; If dn represents the nth digit of the fractional part, find the value of the following expression.
;; d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000
;;
;; Answer = 210

(defn euler-40 []
  (loop [concat-int (apply str (range 1e5))]
    (if (> (count concat-int) 1e6)
      (reduce * (map #(Character/getNumericValue %) (map #(nth concat-int %) [1 1e2 1e3 1e4 1e5 1e6])))
      (recur (apply str (range (+ (count concat-int) 1e5)))))))
