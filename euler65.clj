#!/usr/bin/env cake
;;
;; euler65.clj
;;
;;

;; The infinite continued fraction can be written, 2 = [1 ;(2)], (2) indicates that 2 repeats ad infinitum.
;; In a similar way, 23 =[4;(1,3,1,8)].
;;
;; It turns out that the sequence of partial values of continued fractions for square roots provide the best
;; rational approximations. Let us consider the convergents for 2.
;;
;; Hence the sequence of the first ten convergents for 2 are:
;; 1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
;;
;; What is most surprising is that the important mathematical constant,
;; e =[2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
;; The first ten terms in the sequence of convergents for e are:
;; 2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
;; The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.
;; Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.

;; Convergent Recursion Equations

;; cf = [b0 ,b1 ,b2 ,b3,...] , where cf = N_i/D_i

;; N(i+1) = b(i+1)*N(i) + N(i - 1);   N(1) = b(1)*b(0) + 1;   N(0) = b(0)
;; D(i+1) = b(i+1)*D(i) + D(i - 1);   D(1) = b(1) ;   D(0) = 1

;(use 'sage)
(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

(def & comp)
(def p partial)

(def sqrt clojure.math/sqrt)
(def floor clojure.math/floor)

(defn N-convergents [cf]
  ;; N(i+1) = b(i+1)*N(i) + N(i - 1);   N(1) = b(1)*b(0) + 1;   N(0) = b(0)
  (let [b0 (first cf),
        b1 (second cf),
        bs (nthrest cf 2),
        N1 (+ (* b0 b1) 1 ),
        N0 b0 ]
    (->> (iterate (fn [[N_i, N_i-1, bs]] [(+ (* (first bs) N_i) N_i-1) , N_i, (rest bs)])
                  [N1, N0, bs])
         (take (dec (count cf)))
         (map first)
         (cons N0))))

(defn D-convergents [cf]
  ;; D(i+1) = b(i+1)*D(i) + D(i - 1);   D(1) = b(1) ;   D(0) = 1
  (let [b0 (first cf),
        b1 (second cf),
        bs (nthrest cf 2),
        D1 b1,
        D0 1]
    (->> (iterate (fn [[D_i, D_i-1, bs]] [(+ (* (first bs) D_i) D_i-1) , D_i, (rest bs)])
                  [D1, D0, bs])
         (take (dec (count cf)))
         (map first)
         (cons D0))))

(defn convergents [cf]
  (assert (> (count cf) 1) "cf should be of form [b0,b1...]")
  (map / (N-convergents cf) (D-convergents cf)))

(defn cf-e-alt [n]
  (->> (iterate (fn [[k, cf]] [(inc k) (concat cf [1 (* 2 k) 1])])
                [1 [2]])
       (map second)
       (filter #(>= (count %) n))
       (first)
       (take n)
       (map bigint)))

(defn euler65 [n]
  (->> (N-convergents (cf-e n))
       (last)      
       (cbd/integer->digits)
       (reduce + )))
 
 



