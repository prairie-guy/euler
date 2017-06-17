;;
;; euler88.clj
;;

(require '[cbd])
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
(require '[clojure.pprint :refer (pprint)])
(require '[clojure.set])
(require '[clojure.data.priority-map :refer (priority-map)])
(require '[clojure.core.matrix :as mx :refer (pm mget mset!)])
;(require '[clojure.core.matrix.operators :as m ])
;(mx/set-current-implementation :vectorz)
;(mx/set-current-implementation :ndarray)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Define ps(k,p), a product-sum, as a set (with possible duplication of elements) of natural numbers,
;; {a_1, ... a_k-1, a_k}, where the product and sum of the elements in the set are equal,
;; k >1 and a_1 >= a_2 ... >= a_k, i.e., Sum(a_i) = Product(a_i) for {1 <= i <=k}.
;;
;; It can be shown that ps(k,p) is uniquely characterized by k and p, where k is the number of elements in the set
;; and p is the product (and sum) of the elements in the set.
;;
;; The ps(p,k) set may sometimes also be called the ps(p,k) number for convinience.
;;
;; ps(k,p), for 1 < k < 8 are:
;;
;; ps(2,4)  = {2,2},
;; ps(3,6)  = {3,2,1}
;; ps(4,8)  = {4,2,1,1}
;; ps(5,10) = {5,2,1,1,1}
;; ps(5,9)  = {3,3,1,1,1}
;; ps(5,8)  = {2,2,2,1,1}
;; ps(6,12) = {6,2,1,1,1,1}
;; ps(7,14) = {7,2,1,1,1,1,1}
;; ps(7,12) = {4,3,1,1,1,1,1}
;;
;; For a specific k, there can be multiple p's.
;;
;; Define the set A(k'), to be all the sets ps(k',p) for a specific k'.
;;
;; Define a(k') to be the cardinality (number) of the sets in A(k').
;;
;; For a specific k', the minimal p for all elements in A(k') is mps(k').
;; The minimal ps(k',p) and mps(k') for 1< k'< 8 are:
;;
;; ps(2,4)  = {2,2},                mps(2) = 4
;; ps(3,6)  = {3,2,1}               mps(3) = 6
;; ps(4,8)  = {4,2,1,1}             mps(4) = 8
;; ps(5,8)  = {2,2,2,1,1}           mps(5) = 8
;; ps(6,12) = {6,2,1,1,1,1}         mps(6) = 12
;; ps(7,12) = {4,3,1,1,1,1,1}       mps(7) = 12
;;
;;
;; Finally, consider the sum of unique mps(k) over a range of k, to be Sum {mps(k)}, where {mps(k)} is a set
;; without duplication over a range of k.
;;
;; For example, the set of mps(k) for 2≤k≤12 is {4, 6, 8, 12, 15, 16}, the sum is 61.
;;
;; For problem 88, find the sum of unique mps(k) for {2<= k<= 12000}.
;;
;;
;; Solution Strategy:
;;
;; For any set of factors, whose product is p', it is allways possible to create a set ps(k,p'), where p' is
;; specified but, k is not. To do so, calculate a product, p, and sum, s, from the factors. The difference (p - k),
;; represents the number of 1's that must be added to set of factors. The number of original factors and the
;; number of 1's is k, the number of elements in ps(k,p').
;;
;; For example, for the set of factors {2,2,2}, the product is 8, the sum 6, so 2 1's are added to the original
;; set to result in ps(5,8) = {2,2,2,1,1}.
;;
;; For a given product, p', it is possible to generate all possible ps(k,p'), by finding all of the factorizations
;; of p, (both the prime and non-prime factorizations). The corresponding k for each factorization can then
;; be calcuated (but not without first having the factorization.) Therefore, for a given p', it is possible
;; to find all ps(k,p').
;;
;; However, for a given, k', it is not obvious how to calculate all possible ps(k',p). This is because k is
;; calculated from p' (and a specific factorization).
;;
;; We can however, establish a relationship between the size of p and k, namely that for any ps(k,p), p can
;; be no larger than 2k, i.e., p <= 2k. This is because, for a specific k', there allways exists a
;; ps(k',2k') = {k,2,1,...1}, where there are (k - 2) 1's, i.e., (2k - (k + 2)) = (k - 2). p cannot be larger
;; than 2k', because any rearrangement of the non-unitary values, namely k and 2 in {k,2,1,...,1} will reduce
;; the product to less than 2k', i.e., (k-1) (3) < 2k.
;;
;; Hence to solve the problem, consider all p in the range { 1 < p' < 12,000}, generate all possible factorizations
;; of p', and then generate all ps(k,p'), where k is calculated. Create a hash-map keyed to k.
;; Search through the hash-map for the mps(k) for each k. Find the unique mps(k) and then the sum. This should
;; be the answer.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-ps
  "Takes a set of factors and returns a product-sum, ps,  [k,prod,factors,ones]
  For any sequence of factors, whose product is p, it is possible to generate a product-sum number by padding
   the sequence with 1's equal to the difference between the product and the sum of the sequences."
  [factors]
  (let [prod (apply * factors ), sum (apply + factors), ones  (- prod sum), k (+ (count factors) ones) ]
    [k, prod, (sort > factors,) ones]))

(defn display-ps
  "Takes a ps returned by make-ps and displays it in the form (a_1, a_2,....)"
  [ps]
  (conj (vec (ps 2)) [(ps 3)] ))

(defn factorizations
  "This function, takes a given p and generates all possible factorizations of p. If n is prime, '() is returned.
   A prime-factorization of p is generated, which is then partitioned into all possibilities. (The partition of all
   elements all of length 1, the last partition in this implemenation of combinatorics/partitions, is ignored.)
   For example: n=12 -> (([3 2 2]) ([2 2] [3]) ([3 2] [2])) -> ((3 2 2) (4 3) (6 2))"
  [p]
  (let [partitions-of-prime-factors (butlast (combinatorics/partitions (cbd/factors p))) ]
    (map
     (fn [fact]
       (if (= (count fact) 1)
         (flatten fact)
         (map #(apply * %) fact)))
     partitions-of-prime-factors)))

(defn make_ps_k [k_max]
  (->> (for [p (range 2 (inc (* 2 k_max)))]  (map make-ps (factorizations p)))
       (remove empty?)
       (apply concat)
       (sort-by first)
       (group-by first)))

(defn euler88 [k_max]
  (let [ps_k (make_ps_k k_max)]
    (reduce +
            (distinct
             (for [k (range 2 (inc k_max))]
               (first (sort (map second (ps_k k)))))))))

;; Extra; Not part of the assignment

(defn ps
  "For a given k', define ps(k') to be all sets of the form ps(k',p)."
  [k]
  (map display-ps ((make_ps_k (* 2 k)) k)))

(defn a
  "For a given k', define a(k') to be the number of the sets of the form ps(k',p)."
  [k]
  (count ((make_ps_k (* 2 k)) k)))

(defn N-range
  "Return a range of a(k)"
  [start, end]
  (let [ps_k (make_ps_k (* 2 end))]
    (for [k (range start end)]
      [k (count (ps_k k))])))
