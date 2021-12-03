(use 'tupelo.core)
(use 'cbd)
(require '[clojure.math.numeric-tower :as cmath])
(require '[clojure.math.combinatorics :as comb])


;; Euler 357 - Prime generating integers
;; Consider the divisors of 30: 1,2,3,5,6,10,15,30.
;; It can be seen that for every divisor d of 30, d+30/d is prime.

;; Find the sum of all positive integers n not exceeding 100 000 000
;; such that for every divisor d of n, d+n/d is prime.


(defn divisor-sum-pairs [n]
  (it->
   (map list (divisors n) (reverse (divisors n)))
   (filter #(prime? (+ (first %) (second %))) it)
   (map sort it)
   (distinct it)))

(defn divisor-sum-pairs-prime?
  "(filter divisor-sum-pairs-prime? (range 1 50))"
  [n]
  (it->
   (map list (divisors n) (reverse (divisors n)))
   (map #(prime? (+ (first %) (second %))) it)
   (distinct it)
   (= '(true) it)))

(defn d-not-divides-n_or_sum-prime?
  [d n]
  (or (not= 0 (mod n d)) (prime? (+ d (/ n d)))))

(defn sieve [max]
  (loop [divs (range 2 (cmath/ceil (cmath/sqrt max)))
         filtered (map dec (primes-up-to max))]
    (prn [(first divs)
          ;filtered
          (map #(vector (first divs) (/ % (first divs)))
           (into (sorted-set)(difference (set filtered)
                                         (set (keep-if #(prime? (+ (first divs) (/ % (first divs))))
                                                       filtered)))))])
    (if (empty? divs)
        filtered
        (recur
         (rest divs)
         (keep-if #(d-not-divides-n_or_sum-prime? (first divs) %)
                 filtered)))))
