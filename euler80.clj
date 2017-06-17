;;
;; euler80.clj
;;

(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])
(require 'sage)

(def sqrt clojure.math/sqrt)
(def floor clojure.math/floor)

;; It is well known that if the square root of a natural number is not an integer, then it is
;; irrational. The decimal expansion of such square roots is infinite without any repeating
;; pattern at all.
;;
;; The square root of two is 1.41421356237309504880..., and the digital sum of the first one
;; hundred decimal digits is 475.
;;
;; For the first one hundred natural numbers, find the total of the digital sums of the first
;; one hundred decimal digits for all the irrational square roots.


;; NOTE: THIS CODE WILL NOT WORK FOR NUMBERS GREATER THAN 100!!!!

;; For prototype, use the sage interface to compute the arbitrary length of a square root
(def from-sage (sage/sage))

(def DIGITS 100)                                 ;; Digits include those to lhs of decimal.
(def DIGITS_CALC (+ 2 DIGITS))                   ;; Avoids rounding problems.

(defn range-sqrts [max]
  (let [sqrts (from-sage :value (format "[n(sqrt(i),digits=%d) for i in range(1,%d)]", DIGITS_CALC, max))]
    (from-sage :logout)
    (from-sage :stop-server)
    (read-string
     (clojure.string/replace
      (clojure.string/replace sqrts #"\," "M")   ;; Make these bigints
      #"]" , "M]"))))                            ;; Last number is not followed by a 'comma'

(defn frac [n]
  (if (zero? (- n (floor n)))
    0
    (/ n 10)))

(defn digits-of-frac [f]
  (let [ds (cbd/integer->digits
            (floor (* (clojure.math/expt 10 DIGITS_CALC) (frac f))))]
    (if (>= (count ds) DIGITS)
       (take DIGITS ds)
       ds)))

(defn euler80 [max]
  (reduce + (map #(reduce + %) (map digits-of-frac (range-sqrts max)))))

;; (euler80 100) -> 40886


