;;#!/usr/bin/env cake
;;
;; euler36.clj
;;
;;
;; The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
;;
;; Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
;;
;; (Please note that the palindromic number, in either base, may not include leading zeros.)
;;
;; Answer =  [872187 (1 3 5 7 9 33 99 313 585 717 7447 9009 15351 32223 39993 53235 53835 73737 585585)]


(use '[cbd])
(use 'clojure.contrib.math)

(defn euler [size]
  (for [n (range 1 (inc size))
        :when (let [b-2 (digits (Integer/toString n 2))]
                (and
                 (= (digits n) (reverse (digits n)))
                 (= b-2 (reverse b-2))))]
    n))

(defn run []
  (let [result (euler 1e6)]
    [(reduce + result) result]))

                 

                
