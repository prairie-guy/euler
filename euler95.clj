;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; euler95.clj
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
;;(mx/set-current-implementation :ndarray)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The proper divisors of a number are all the divisors excluding the number itself. For example,
;; the proper divisors of 28 are 1, 2, 4, 7, and 14. As the sum of these divisors is equal to 28,
;; we call it a perfect number.Interestingly the sum of the proper divisors of 220 is 284 and the sum of
;; the proper divisors of 284 is 220, forming a chain of two numbers. For this reason, 220 and 284 are
;; called an amicable pair.
;;
;; Perhaps less well known are longer chains. For example, starting with 12496, we form a chain of five numbers:
;;
;; 12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)
;;
;; Since this chain returns to its starting point, it is called an amicable chain.
;;
;; Find the smallest member of the longest amicable chain with no element exceeding one million.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn sum-of-divisors [n] (reduce + (cbd/proper-divisors n)))

(def Successor (into {} (for [k (range 1 (inc 1000000))
                                :let [sod (sum-of-divisors k)]] 
                          [k, (if (<= sod 1000000) sod 0)])))

(defn chain [k]
  (loop [i k, result []]
    (cond
      (== (Successor i) 0) result
      

          )
    )
  )


