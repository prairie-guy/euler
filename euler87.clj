;;
;; euler87.clj
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
;; The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28.
;; In fact, there are exactly four numbers below fifty that can be expressed in such a way:
;;
;; 28 = 2^2 + 2^3 + 2^4
;; 33 = 3^2 + 2^3 + 2^4
;; 49 = 5^2 + 2^3 + 2^4
;; 47 = 2^2 + 3^3 + 2^4
;;
;; How many numbers below fifty million can be expressed as the
;; sum of a prime square, prime cube, and prime fourth power?
;;
;; Power(n)   1/n-power(50 million)  Number Primes
;;   2              7072                  908
;;   3               369                   73
;;   4                85                   23
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn euler87 [max]
  (time
   (let [sq-primes (cbd/primes-up-to (ceil (Math/pow max (/ 1 2))))
         cb-primes (cbd/primes-up-to (ceil (Math/pow max (/ 1 3))))
         frth-primes (cbd/primes-up-to (ceil (Math/pow max (/ 1 4))))]
     (count
      (distinct
       (for [sq sq-primes, cb cb-primes, fth frth-primes
             :let [sum (+ (Math/pow sq 2), (Math/pow cb 3), (Math/pow fth 4))]
             :when (< sum  max)]
         sum))))))


;;(euler87 50000000) "Elapsed time: 3333.413 msecs" -> 1097343
