;;
;; euler75.clj
;;

;; It turns out that 12 cm is the smallest length of wire that can be bent to form an
;; integer sided right angle triangle in exactly one way, but there are many more examples.

;; 12 cm: (3,4,5)
;; 24 cm: (6,8,10)
;; 30 cm: (5,12,13)
;; 36 cm: (9,12,15)
;; 40 cm: (8,15,17)
;; 48 cm: (12,16,20)

;; In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided
;; right angle triangle, and other lengths allow more than one solution to be found; for
;; example, using 120 cm it is possible to form exactly three different integer sided right
;; angle triangles.

;; 120 cm: (30,40,50), (20,48,52), (24,45,51)

;; Given that L is the length of the wire, for how many values of L <= 1,500,000 can exactly
;; one integer sided right angle triangle be formed?

;; Euclid's formula is a fundamental formula for generating Pythagorean triples given
;; an arbitrary pair of positive integers m and n with m > n. The formula states that
;; the integers: a = m^2-n^2, b=2*m*n, c=m^2+n^2


(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

;(def sqrt clojure.math/sqrt)
;(def floor clojure.math/floor)

(defn coprime? [m n]
  (= 1 (clojure.math/gcd n m)))

(defn prim-triples [M N]
  ;; Generate all primitive pythagorean triples for N and M, with M > N.
  (distinct
   (map sort
        (for [n (range 1 (inc N)), m (range (inc n) (inc M)) :when (coprime? n m)]
          (if (odd? (- m n))
            [(- (* m m) (* n  n)), (* 2 n m), (+ (* m m) (* n n))]
            [(/ (- (* m m) (* n  n)) 2), (/ (* 2 n m) 2), (/ (+ (* m m) (* n n)) 2)])))))

(defn prim-triples<= [L]
  ;; Generate all primitive pythagorean triples with sum of sides <= L.
  (let [h (clojure.math/ceil (sqrt L))]
    (filter #(<= (reduce + %) L)
            (prim-triples h h))))

(defn multiple-triples<= [L triple]
    ;; Generate all multiples of a particular primitive pythagorean triple <= L.
  (take-while #(<= (reduce + %) L)
              (for [k (iterate inc 1)] (map #(* k %) triple))))

(defn all-triples<= [L]
  ;; Generate all pythagorean triples with sum of sides <= L.
  (partition 3
   (flatten
    (map #(multiple-triples<= L %)  (prim-triples<= L)))))

(defn euler66 [L]
  (count
   (filter #(= 1 (last %))
           (frequencies
            (sort
             (map #(reduce + % ) (all-triples<= L)))))))

;; (euler66 1500000) -> 161667
