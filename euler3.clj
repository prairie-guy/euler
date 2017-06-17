#!/usr/bin/env cake

;; euler3.clj
;; Find the largest prime factor of a composite number.
;; 
;;

(defn sieve [stream]
  (lazy-seq (cons (first stream)
                  (sieve (filter #(not (zero? (mod % (first stream)))) (rest stream))))))

(def naturals (iterate inc 2))

(def primes-stream (sieve naturals))

(defn factors [num]
  (if (= num 0)
    []
    (let [max (Math/sqrt num)]
      (loop [n 2, rem num, primes []]
        (cond
         (= rem 1) primes
         (= (mod rem n) 0) (recur n (quot rem n) (conj primes n))
         (> n max) (conj primes rem)
         :else (recur (inc n) rem primes))))))


;(prn (factors 600851475143))

(defn take-primes []
  (if (= num 0)
    []
    (loop [n 2, rem num, primes []]
      (cond
       (= rem 1) primes
       (= (mod rem n) 0) (recur n (quot rem n) (conj primes n))
       (> n max) (conj primes rem)
       :else (recur (inc n) rem primes))))))