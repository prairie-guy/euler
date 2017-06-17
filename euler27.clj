#!/usr/bin/env cake
;;
;; euler27.clj
;;
;; Considering quadratics of the form:
;;
;; n^2 + an + b, where |a|  1000 and |b|  1000
;;
;; where |n| is the modulus/absolute value of n e.g. |11| = 11 and |4| = 4
;;
;; Find the product of the coefficients, a and b, for the quadratic expression that
;; produces the maximum number of primes for consecutive values of n, starting with n = 0.
;;
;; Answer = -59231 


(use '[cbd])
(use 'clojure.contrib.math)
(use '[clojure.contrib.lazy-seqs :only (primes)])

(def primes-map (apply hash-map (flatten (map vector (take 10000 primes) (repeat true)))))

(defn prime? [n] (get primes-map n false))

(defn quadratic [a b] (fn [n] (+ (* n n) (* a n) b)))

(defn run-of-primes [a b] (count (take-while prime? (map (quadratic a b)  (iterate inc 0)))))
(defn show-run-of-primes [a b] (take-while prime? (map (quadratic a b)  (iterate inc 0))))

(defn largest-run-of-primes [size]
  (reduce (fn [x y] (if (> (second x) (second y)) x y))
   (for [a (range (- size) size), b (range (- size) (inc size))] [[a b] (run-of-primes a b)])))

(defn euler27 []
  (apply * (first (largest-run-of-primes 1000))))