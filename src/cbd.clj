;; CBD Module for Various Personal Utilities
;; 3/2/2021

(ns cbd
  (:use [clojure.math.numeric-tower :only (round sqrt ceil floor gcd)])
  (:use [clojure.math.combinatorics :only (partitions)])
  (:use [tupelo.core :only (it-> append)])
  )

(defn integer->digits [integer] (map #(- (int %) 48) (str integer)))
(defn digits->integer [digits] (bigint (apply str digits)))
(defn integer-count   [integer] (count (str (bigint integer))))
(defn third [lst] (nth lst 2))

; A lazy sequence of the natural numbers
(def
  naturals (map inc (range)))

(defn remove-n
  "Returns a lazy sequence of the items in coll for which
  first-n (pred item) returns true have been removed.
  pred must be free of side-effects."
  [n pred coll]
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

(defn comb
  "Returns the number of ways of choosing 'choose' items from 'n' items."
  [n choose]
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
   (if (< n 1)  (throw (Exception. "(factors n) error: n <1"))
    (factors [] n 2)))
  ([facts n candidate]
     (cond
      (= n 1) facts
      (= 0 (rem n candidate)) (recur (conj facts candidate) (quot n candidate) candidate)
      (> candidate (Math/sqrt n)) (conj facts n)
            :else (recur facts n (inc candidate)))))

(defn coprime?
  "n and m are coprime iff 1 is the only shared common factor."
  [m n]
  (= 1 (gcd n m)))


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


(defn proper-divisors
  "proper-divisors do not include themselves.
   For example (proper-divisors 12) -> (1 2 3 4 6)"
  [n]
  (let [sqrt-n (int (Math/sqrt n))
        divs-upto-sqrt (filter #(zero? (rem n %)) (range 2 (inc sqrt-n)))
        rest-of-divs (reverse (map #(/ n %) divs-upto-sqrt))]
    (concat '(1) divs-upto-sqrt
            (if (= n (* sqrt-n sqrt-n))
              (drop 1 rest-of-divs)
              rest-of-divs))))


(defn divisors
  "divisors include themselves.
  For example, (divisors 12) -> (1 2 3 4 6 12)"
  [n]
  (concat (proper-divisors n) (list n)))

;; https://mathworld.wolfram.com/PartitionFunctionQ.html
(defn sigma_0
  "The divisor function sigma_k(n) for n=0 is defined as the number of
  the (positive integer) divisors of n
  "
  [n]
  (->> (factors n)
       (frequencies)
       (vals)
       (map inc)
       (reduce +)
       (inc)))

(defn sigma_1
  "The divisor function sigma_k(n) for n=1 is defined as the sum of
  the (positive integer) divisors of n
  https://mathworld.wolfram.com/PartitionFunctionQ.html"
  [n]
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
  "Lazy sequence of primes"
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                (dissoc candidate)
                (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate
                (lazy-seq (next-primes (next-sieve sieve candidate)
                            (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))


(defn prime?
  "Is n prime?"
  [n]
  (let [max (sqrt n)]
    (not-any? #(zero? (rem n %)) (take-while #(<= % max) primes))))


(defn sum [ns] (reduce +  0N ns))

(defn prod [ns] (reduce * 1N ns))

(defn summation
  "summation either returns the sums the numbers over [from to] or
  returns the sum after applying a function, fn[k], over [from to].
  For example (summation 1 5 (fn[k] (* k k)) -> 55"
  ([from to]   (reduce + 0 (map + (range from (inc to)))))
  ([from to f] (reduce + 0 (map f (range from (inc to))))))

(defn product
  "product either returns the product of the  numbers over [from to] or
  returns the product after applying a function, fn[k], over [from to].
  For example (summation 1 5 (fn[k] (* k k)) -> 55"
  ([from to]   (reduce * 1 (map + (range from (inc to)))))
  ([from to f] (reduce * 1 (map f (range from (inc to))))))

(defn phi
  "The totient function phi(n), also Euler's totient function,
   is the number of positive integers <=n relatively prime to n
   (i.e., no factors in common with n), where 1 is relatively prime."
  [n]
  (* n (prod (map #(- 1 (/ 1N %)) (distinct (factors n))))))

;; https://mathworld.wolfram.com/Primorial.html
(defn primorial
  "Returns the product of the first k-primes"
  [k]
  (prod (take k primes)))


(defn partitions-lexical
  "Returns the lexical partitions of the integer n"
  [n]
  (if (zero? n)
    '(())
    (apply concat
      (for [p (partitions-lexical (dec n))]
        (let [res [(cons 1 p)]]
          (if (and (not (empty? p))
                   (or (< (count p) 2) (> (second p) (first p))))
            (conj res (cons (inc (first p)) (rest p)))
            res))))))



(defn- partitions-k
  "k-th iteration of partition-n
  ps(k) is a vector memoizing previous paritions,
  ps = [1,1,2,3,5...]. (ps k) -> kth value of ps"
  [n ps]
  (* (/ 1 n)
     (summation 0 (dec n)
                (fn [k] (* (sigma_1 (- n k)) (ps k))))))

(defn partitions-n
 "Calculate the number of partion integers of n
  https://mathworld.wolfram.com/PartitionFunctionP.html (13)"
  [n]
  (it-> (iterate
         (fn [[k ps]] ; ps is an array of previous partitionsy
           [(inc k) (conj ps (partitions-k k ps))])
         [1 [1]])
        (nth it n)
        (last (last it))))

(defn round-decimals
  "Round a floating point number to N decimal places, returning a double.
     (round-decimals 3.14156 2) => 3.14"
  [N val]
  (let [factor (Math/pow 10.0 (double N))]
    (it-> (double val)
      (* it factor)
      (Math/round it)
      (/ it factor))))
