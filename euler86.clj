;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; euler86.clj
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '[cbd])
(require '[discrete])
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
(require '[clojure.pprint :refer (pprint)])
(require '[cuerdas.core :as st])
(require '[clojure.core.match :refer [match]])
(require '[defun :refer [defun]])

;;(require '[clojure.set])
;;(require '[clojure.data.priority-map :refer (priority-map)])
;;(require '[clojure.core.matrix :as mx :refer (pm mget mset!)])
;;(require '[clojure.core.matrix.operators :as m ])
;;(mx/set-current-implementation :vectorz)
;;((mx/set-current-implementation :ndarray)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a fly, F, sits in the opposite
;; corner. By travelling on the surfaces of the room the shortest "straight line" distance from S to F is 10 and
;; the path is shown on the diagram.
;; However, there are up to three "shortest" path candidates for any given cuboid and the shortest route
;; doesn't always have integer length.
;;
;; By considering all cuboid rooms with integer dimensions, up to a maximum size of M by M by M, there are
;; exactly 2060 cuboids for which the shortest route has integer length when M=100, and this is the least value
;; of M for which the number of solutions first exceeds two thousand; the number of solutions is 1975 when M=99.
;;
;; Find the least value of M such that the number of solutions first exceeds one million.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn coprime? [m n]
  (= 1 (gcd n m)))


(defn primitive-triples [M N]
  ;; Generate all primitive pythagorean triples for N and M, with M > N.
  (distinct
   (map sort
        (for [n (range 1 (inc N)) , m (range (inc n) (inc M)) :when (coprime? n m)]
          (if (odd? (- m n))
            [(- (* m m) (* n  n)), (* 2 n m), (+ (* m m) (* n n))]
            [(/ (- (* m m) (* n  n)) 2), (/ (* 2 n m) 2), (/ (+ (* m m) (* n n)) 2)])))))


(defn pythagorean-triples [L side primitive]
  "Returns all :[p]rimitive or [:all] pythagorean triples up to a limit for sides :a, :b, :ab, or :c
   pythagorean-triples 100 :c :p
   pythagorean-triples 100 :a :all"
  (let [max (cond
              ;;(= side :a)  (* 2 L)
              (= side :a)  (ceil (/ L 2))
              (= side :b)  (* 2 (ceil (sqrt L)))
              (= side :ab) (* 2 L)
              (= side :c) (ceil (sqrt L))
              :else (ceil (sqrt L)))
        side? (cond
                (= side :a)  #(<= (nth % 0) L)
                (= side :b)  #(<= (nth % 1) L)
                (= side :ab)  #(and (<= (nth % 0) L) (<= (nth % 1) L))
                (= side :c) #(<= (nth % 2) L)
                :else #(<= (nth % 2) L))
        sort-order (cond
                     (= side :a)  first
                     (= side :b)  second
                     (= side :ab) (juxt first second)
                     (= side :c) last
                     :else last)]
    (if (not(= primitive :all))
      (sort-by sort-order (filter side? (primitive-triples max max)))
      (sort-by sort-order (partition 3 (flatten
                                        (map #(take-while side?
                                                          (for [k (iterate inc 1)]
                                                            (map (fn [triple] (* k triple)) %)))
                                             (filter side? (primitive-triples max max)))))))))


(defn cuboids [[a b c]]
  "Turn a triple [a, b,c] into cuboids: [a, i, (-b i)]. For example (cuboids [3 4 5]) -> ([3 1 3] [3 2 2] [3 3 1])"
  (for [i (range 1 b)] [a, i, (- b i)]))

(defn both [[a b c]]
  "Each triple has two forms, each producing seperate sets of cuboids "
  [[a b c],[b a c]])

(defn all-comb-triples [L side primitive]
  (partition 3 (flatten(map both (pythagorean-triples L side primitive)))))

(defn all-cuboids [L side primitive]
  (filter #(<= (last %) L)
          (distinct (map sort (partition 3(flatten(map cuboids (all-comb-triples L side primitive))))))))

(defn shortest? [[a b c]]
  "For a cuboid [a b c], determine if shortest path is an integer."
  (let [ s (apply min [(Math/hypot (+ a b) c), (Math/hypot a (+ b c)), (Math/hypot (+ a c) b)])]
    (== (Math/floor s) s)))

(defn euler86-test [L]
  (count (filter #(= true %)(map shortest? (all-cuboids L :a :all)))))
