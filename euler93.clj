;;
;; euler93.clj
;;

(require '[cbd])
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
(require '[clojure.pprint :refer (pprint)])
;;(require '[clojure.set])
;;(require '[clojure.data.priority-map :refer (priority-map)])
;;(require '[clojure.core.matrix :as mx :refer (pm mget mset!)])
;;(require '[clojure.core.matrix.operators :as m ])
;;(mx/set-current-implementation :vectorz)
;;(mx/set-current-implementation :ndarray)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and making use of the four arithmetic
;; operations (+, −, *, /) and brackets/parentheses, it is possible to form different positive integer targets.
;;
;; For example,
;;
;; 8 = (4 * (1 + 3)) / 2
;; 14 = 4 * (3 + 1 / 2)
;; 19 = 4 * (2 + 3) − 1
;; 36 = 3 * 4 * (2 + 1)
;;
;; Note that concatenations of the digits, like 12 + 34, are not allowed.
;;
;; Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of which 36
;; is the maximum, and each of the numbers 1 to 28 can be obtained before encountering the first non-expressible number.
;;
;; Find the set of four distinct digits, a < b < c < d, for which the longest set of consecutive positive
;; integers, 1 to n, can be obtained, giving your answer as a string: abcd.
;;
;;
;; Solution:
;;
;; For a set of 4 digits, there are 5 ways to associate the digits with parentheses.
;; (a(b(cd)))
;; (((ab)c)d)
;; ((a(bc))d)
;; (a((bc)d))
;; ((ab)(cd))

;; In lisp, this naturally leaads to 5 possible ways to associate 3 operators and 4 arguments.
;; (make-expressions ['o1 'o2 'o3] ['a 'b 'c 'd]) ->
;; (o1 a (o2 b (o3 c d)))
;; (o1 (o2 (o3 a b) c) d)
;; (o1 (o2 a (o3 b c)) d)
;; (o1 a (o2 (o3 b c) d))
;; (o1 (o2 a b) (o3 c d))

;; Expressions: (selections [+ - * div] 3)) * (permutations [a b c d]) * 5 * (upper-triangle)
;;                                       64 * 24 * 5 * 210 = 1,612,800

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn make-expressions [operators arguments]
  "Debugging tool to make sure expressions were properly composed"
  (let [[o1 o2 o3] operators,
        [a b c d] arguments]
    [[o1 o2 o3] [a b c d]]
    [(list o1 a (list o2 b (list o3 c d)))
     (list o1 (list o2 (list o3 a b) c) d)
     (list o1 (list o2 a (list o3 b c)) d )
     (list o1 a (list o2 (list o3 b c) d))
     (list o1 (list o2 a b) (list o3 c d))]))


(defn div [a b]
  "Need to redefine '/' as div to catch division by 0. If caught, returns -1"
  (if (nil? (try (/ a b) (catch Exception e)))
    -1
    (/ a b)))

(defn sequence-length [sorted-series]
  "Calculates the length of an integer sequence starting at 1"
  (count (take-while zero? (map - (rest (range)) sorted-series))))

(defn eval-expressions [arguments]
  "Takes a set of 4 distinct digits <- {0-9}.
   Creates a sorted list of distinct, positive integers in which all permutations of the 4 digits of the set,
   (3 of 4) possible operators are evaluated against all 5 expressions that can be formed by parenthetical association.
   For each sorted-list, the sequence-length is computed, and a vector of it and the arguments are returned."
  [(sequence-length
    (sort
     (distinct
      (filter #(and (integer? %) (pos? %))
              (flatten
               (for [[o1 o2 o3] (combinatorics/selections [+ - * div] 3),
                     [a b c d] (combinatorics/permutations arguments)]
                 [(o1 a (o2 b (o3 c d)))
                  (o1 (o2 (o3 a b) c) d)
                  (o1 (o2 a (o3 b c)) d )
                  (o1 a (o2 (o3 b c) d))
                  (o1 (o2 a b) (o3 c d))])))))),
   arguments])

(defn generate-sets []
  "Returns all possible sets of {a,b,c,d} such that each digit {0-9} is distinct and, a < b < c < d"
  (combinatorics/combinations (range 10) 4))

(defn euler93 []
  (time
   (last (sort-by first
     (for [arguments (generate-sets)]
       (eval-expressions arguments))))))


;; (euler93) "Elapsed time: 2506.14 msecs"->  [51 (1 2 5 8)]
