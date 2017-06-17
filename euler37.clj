#!/usr/bin/env cake
;;
;; euler37.clj
;;
;;

(use '[cbd])
(use 'clojure.contrib.math)

;; The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right,
;; and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.
;;
;; Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
;;
;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
;;
;; Answer = [11 748317 (23 37 53 73 313 317 373 797 3137 3797 739397)]


(defn slice-up [number]
  (let [n (digits number), len (count n)]
    (map digits->integer (concat (partition-all len 1 n)
                                 (map reverse (partition-all len 1 (reverse n)))))))

(defn euler37 [size]
  (let [prime? (apply hash-set (primes-up-to size))]
    (filter #(not (#{2 3 5 7} %))
            (for [n (range (inc size)) :when (not (some nil? (map prime? (slice-up n))))] n))))

(defn run[]
  (let [result (euler37 1e6)]
    [(count result) (reduce + result) result]))