;;
;; euler97.clj
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
;;
;; The first known prime found to exceed one million digits was discovered in 1999, and is a Mersenne
;; prime of the form 2^6972593−1; it contains exactly 2,098,960 digits. Subsequently other Mersenne primes,
;; of the form 2p−1, have been found which contain more digits.
;;
;; However, in 2004 there was found a massive non-Mersenne prime which contains
;; 2,357,207 digits: 28433×2^7830457+ 1.
;;
;; Find the last ten digits of this prime number.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn last-digits-two-pows [e]
  (->>
   (expt 2N e)
   (cbd/integer->digits)
   (take-last 11)
   (cbd/digits->integer)))

(defn euler97-1 []
  "Factor 2^7,830,457 2^30457 * (2^100,000)^780"
  (time
   (->>
    (reduce * (for [e (cons 30457 (repeat 780 10000) )]
                (last-digits-two-pows e)))
    (* 28433 )
    (inc)
    (cbd/integer->digits)
    (take-last 10)
    (cbd/digits->integer))))


;; (euler97-1) "Elapsed time: 1208.954 msecs" -> 8739992577N
