#!/usr/bin/env cake
;;
;; euler4.clj
;; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91  99.
;;
;; Find the largest palindrome made from the product of two 3-digit numbers.
;; 
;;

(require '[cbd])
(use 'clojure.contrib.math)

(defn pali [order]
  (last (sort (for [p
        (for [i (range (expt 10 (- order 1)) (expt 10 order)),
              j (range (expt 10 (- order 1)) (expt 10 order))] (* i j) )
              :when (= (into [] (str p)) (into () (str p)))] p))))                    ;; Relies on how vectors and lists are created with 'into'
;;            :when (= (into [] (str p)) (reverse (str p)))] p)))
;;            :when (= (cbd/integer->list p) (reverse (cbd/integer->list p)))] p))))

(prn (pali 3))