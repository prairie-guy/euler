#!/usr/bin/env cake
;;
;; euler35.clj
;;
;; The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
;;
;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
;;
;; How many circular primes are there below one million?
;;
;; Answer = 55

(use '[cbd])
(use 'clojure.contrib.math)

(defn cyclicals [num]
    (let [n (digits num), size (count n)]
;;    (let [n (integer->list num), size (count n)]
      (map digits->integer (partition size 1 (take (dec (+ size size)) (cycle n))))))

(defn euler35 [size]
  (let [max (inc (Math/pow 10 (Math/ceil (Math/log10 size))))
        prime? (apply hash-set (primes-up-to max))
        circular-primes (for [i (range size) :when (reduce (fn [a b] (and a b))  (map prime? (cyclicals i)))] i)]
    [(count circular-primes) circular-primes]))

(defn run [] (euler35 1e6))


                