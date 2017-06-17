#!/usr/bin/env cake
;;
;; euler66.clj
;;
;;

;; Convergent Recursion Equations
;; cf = [b0 ,b1 ,b2 ,b3,...] , where cf = N_i/D_i
;;
;; N(i+1) = b(i+1)*N(i) + N(i-1);   N(1) = b(1)*b(0) + 1;   N(0) = b(0)
;; D(i+1) = b(i+1)*D(i) + D(i-1);   D(1) = b(1)         ;   D(0) = 1

;; Consider quadratic Diophantine equations of the form:

;; x^2 – K*y^2 = 1
;;

;; For example, when D=13, the minimal solution in x is 6492 – 131802 = 1.
;; It can be assumed that there are no solutions in positive integers when D is square.
;; By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:

;; 3^2 – 2*2^2 = 1
;; 2^2 – 3*1^2 = 1
;; 9^2 – 5*4^2 = 1
;; 5^2 – 6*2^2 = 1
;; 8^2 – 7*3^2 = 1

;; Hence, by considering minimal solutions in x for D  7, the largest x is obtained when D=5.

;; Find the value of D <=1000 in minimal solutions of x for which the largest value of x is obtained.

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
  "cf is a finite or infinite series of bs, i.e., [b0, b1, b2 ...], from the continued fraction expansion.
   Returns a finite or infinite series of D's, representing the denominator of the convergents of cf.
   The recurrance relationship is as follows:
   N(i+1) = b(i+1)*N(i) + N(i-1);   N(1) = b(1)*b(0) + 1;   N(0) = b(0)   "
  (let [b0 (first cf),
        b1 (second cf),
        bs (nthrest cf 2),
        N1 (+ (* b0 b1) 1 ),
        N0 b0 ]
    (->> (iterate (fn [[N_i, N_i-1, bs]] [(+ (* (first bs) N_i) N_i-1) , N_i, (rest bs)])
                  [N1, N0, bs])
         ;; N_i for finite cf are split by (not-empty? bs) and (empty? bs); the later still has the final N.
         (split-with #(not-empty (nth % 2)))
         ((fn [split-by-bs] (lazy-cat (first split-by-bs) (take 1 (second split-by-bs)))))
         (map first)
         (cons N0))))

(defn D-convergents [cf]
  "cf is a finite or infinite series of bs, i.e., [b0, b1, b2 ...], from the continued fraction expansion.
   Returns a finite or infinite series of D's, representing the denominator of the convergents of cf.
   The recurrance relationship is as follows:
   D(i+1) = b(i+1)*D(i) + D(i-1);   D(1) = b(1) ;   D(0) = 1"
  (let [b0 (first cf),
        b1 (second cf),
        bs (nthrest cf 2),
        D1 b1,
        D0 1]
    (->> (iterate (fn [[D_i, D_i-1, bs]] [(+ (* (first bs) D_i) D_i-1) , D_i, (rest bs)])
                  [D1, D0, bs])
         ;; D_i for finite cf are split by (not-empty? bs) and (empty? bs); the later still has the final D.
         (split-with #(not-empty (nth % 2)))
         ((fn [split-by-bs] (lazy-cat (first split-by-bs) (take 1 (second split-by-bs)))))
         (map first)
         (cons D0))))

(defn- iterated-expansion-of-sqrt [n]
  "Auxiliary function generate triplets used to find the continued fraction expansion of the square root of n."
  (iterate
   (fn [[m, d, a ]]
     (let [m (bigint (- (* d a) m)),
           d (bigint (/ (- n (* m m)) d))
           a (bigint (floor (/ (+ (sqrt n) m) d)))]
       [m, d, a]))
   [0, 1, (bigint (floor (sqrt n)))]))

(defn cf-sqrt[n]
  "Returns sqrt of n as a series of finite or infinite series of bs."
  (map #(nth % 2) (iterated-expansion-of-sqrt n)))

(defn cf-sqrt-std [n]
  "Loop through the iterated continued expansion until the triplet [m, d, a] is repeated
   Returns cf in std-form: [(a) (b, c &rest), where (b,c &rest) is finite and repeating."
  (loop [cf (iterated-expansion-of-sqrt n), result []]
    (if (some #{(first cf)}  result)
      (split-at 1 (map last result))
      (recur (rest cf), (conj result (first cf))))))

(defn solve-pell-eq [k]
  "Find the smallest integers {x,y} s.t. x^2 – k*y^2 = 1"
  (assert (not (integer? (sqrt k))))
  (let [pell-eq? (fn [k, x, y] (= 1 (- (* x x) (* k y y))))
        ns (N-convergents (cf-sqrt k))
        ds (D-convergents (cf-sqrt k))]
    (first (for [[x y] (map vector ns ds) :when (pell-eq? k x y)] [x y]))))

(defn euler66 [n]
  (apply (partial max-key second)
         (for [k (range 2 (inc n))
               :when (not (integer? (sqrt k)))
               :let [[x, y] (solve-pell-eq k)]] [k,x,y])))

;; (euler66 1000) -> [661 16421658242965910275055840472270471049N 638728478116949861246791167518480580N]
