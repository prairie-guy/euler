;; CBD Module for Various Personal Utilities
;;


(ns cbd
  (:use [clojure.math.numeric-tower :only (round sqrt ceil floor)])
  (:use [clojure.math.combinatorics :only (partitions)]))

(defn integer->digits [integer] (map #(- (int %) 48) (str integer)))
(defn digits->integer [digits] (bigint (apply str digits)))
(defn integer-count   [integer] (count (str (bigint integer))))

(defn remove-n   [n pred coll]
  "Returns a lazy sequence of the items in coll for which
  first-n (pred item) returns true have been removed.
  pred must be free of side-effects."
  (loop [times n, coll coll, result []]
    (cond (empty? coll) (seq result)
          (= times 0) (concat result coll)
          (pred (first coll)) (recur (dec times) (rest coll) result)
          :else (recur times (rest coll) (conj result (first coll))))))

(defn  fact  [n]
  (if (zero? n) 1
      (loop [i 1N, result 1N]
        (if (== i n) (* i result)
            (recur (inc i) (*' i result))))))

(defn comb [n choose]
  "Returns the number of ways of choosing 'choose' items from 'n' items."
  (if (> choose n) nil
      (/ (fact n)
         (* (fact choose)
            (fact (- n choose))))))

;; With duplication
(defn mix [& lsts]
  (defn mix-helper [lsts result]
    (if (empty? lsts)
      result
      (mix-helper (rest lsts) (for [i (first lsts) j result] (cons i j)))))
  (let [lsts (reverse lsts)]
    (mix-helper (rest lsts) (for [i (first lsts)] (list i)))))

;; No duplication
(defn permute [& lsts]
  (defn permute-helper [lsts result]
    (if (empty? lsts) result
        (permute-helper (rest lsts) (for [i (first lsts), j result :when (not (some #(= i %)  j))] (cons i j)))))
  (let [lsts (reverse lsts)]
    (permute-helper (rest lsts) (for [i (first lsts)] (list i)))))

(defn permutations [choose lst]
  (if (= 0 choose)
    lst
    (apply permute (for [i (range choose)] lst))))

(defn combine [& lsts]
  (defn combine-helper [lsts result]
    (if (empty? lsts) (distinct (map #(sort < %) result))
        (combine-helper (rest lsts) (for [i (first lsts), j result :when (not (some #(= i %)  j))] (cons i j)))))
  (combine-helper (rest lsts) (for [i (first lsts)] (list i))))

(defn combinations [choose lst]
  (if (= 0 choose)
    lst
    (apply combine (for [i (range choose)] lst))))

(defn subsets [lst]
  (if (empty? lst)
    (list '())
    (let [r (subsets (rest lst))]
      (concat r (map (fn [x] (cons (first lst) x)) r)))))

(defn factors
  ([n]
     (factors [] n 2))
  ([facts n candidate]
     (cond
      (= n 1) facts
      (= 0 (rem n candidate)) (recur (conj facts candidate) (quot n candidate) candidate)
      (> candidate (Math/sqrt n)) (conj facts n)
            :else (recur facts n (inc candidate)))))


(defn factorizations
  "This function, takes a given p and generates all possible factorizations of p. If n is prime, '() is returned.
   A prime-factorization of p is generated, which is then partitioned into all possibilities. (The partition of all
   elements all of length 1, the last partition in this implemenation of combinatorics/partitions, is ignored.)
   For example: n=12 -> (([3 2 2]) ([2 2] [3]) ([3 2] [2])) -> ((3 2 2) (4 3) (6 2))"
  [p]
  (let [partitions-of-prime-factors (butlast (partitions (factors p))) ]
    (map
     (fn [fact]
       (if (= (count fact) 1)
         (flatten fact)
         (map #(apply * %) fact)))
     partitions-of-prime-factors)))


(defn proper-divisors [n]
  (let [sqrt-n (int (Math/sqrt n))
        divs-upto-sqrt (filter #(zero? (rem n %)) (range 2 (inc sqrt-n)))
        rest-of-divs (reverse (map #(/ n %) divs-upto-sqrt))]
    (concat '(1) divs-upto-sqrt
            (if (= n (* sqrt-n sqrt-n))
              (drop 1 rest-of-divs)
              rest-of-divs))))


(defn divisors [n]
  (concat (proper-divisors n) (list (round n))))

(defn num-divisors [n]
  (->> (factors n)
       (frequencies)
       (vals)
       (map inc)
       (reduce *)))



(defn primes-up-to [n]
  (let [n (int n)]
    "Returns a list of all primes from 2 to n"
    (let [root (int (Math/round (Math/floor (Math/sqrt n))))]
      (loop [i (int 3)
             a (int-array n)
             result (transient [2])]
        (if (>= i n)
          (persistent! result)
          (recur (+ i (int 2))
                 (if (<= i root)
                   (loop [arr a
                          inc (+ i i)
                          j (* i i)]
                     (if (>= j n)
                       arr
                       (recur (do (aset arr j (int 1)) arr)
                              inc
                              (+ j inc))))
                   a)
                 (if (zero? (aget a i))
                   (conj! result i)
                   result)))))))



(def primes
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
          (fn primes-from [n [f & r]]
            (if (some #(zero? (rem n %))
                      (take-while #(<= (* % %) n) primes))
              (recur (+ n f) r)
              (lazy-seq (cons n (primes-from (+ n f) r)))))
          wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                        6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                        2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn prime? [n]
  (let [max (sqrt n)]
    (not-any? #(zero? (rem n %)) (take-while #(<= % max) primes))))


(defn zip [& lsts] (apply (partial map list) lsts))


(defn sum [a b f]
  (reduce + 0 (map f (range a (inc b)))))

(defn product [a b f]
  (reduce * 1 (map f (range a (inc b)))))
