;;
;; euler160.clj
;;
(require '[discrete])
(require '[cbd])
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
(require '[clojure.pprint :refer (pprint)])
(require '[cuerdas.core :as st])
(require '[clojure.core.match :refer [match]])

;;(require '[clojure.set])
;;(require '[clojure.data.priority-map :refer (priority-map)])
;;(require '[clojure.core.matrix :as mx :refer (pm mget mset!)])
;;(require '[clojure.core.matrix.operators :as m ])
;;(mx/set-current-implementation :vectorz)
;;(mx/set-current-implementation :ndarray)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For any N, let f(N) be the last five digits before the trailing zeroes in N!.
;; For example,
;;
;; 9! = 362880 so f(9)=36288
;; 10! = 3628800 so f(10)=36288
;; 20! = 2432902008176640000 so f(20)=17664
;;
;; Find f(1,000,000,000,000)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn last-sig-digits [n sig]
  "Return last non-zero 'sig' digits from 'N' "
  (cbd/digits->integer
   (reverse
    (take sig (drop-while #(== 0 %)
                          (reverse (cbd/integer->digits n)))))))

(defn dp [fct p]
  "dp: Number of primes 'p' in 'fct', where fct is in factorial form, i.e., fct -> 100 means 100!"
  (reduce +
          (for [k (range 1 (inc (math/floor (/ (Math/log fct) (Math/log p)))))]
            (bigint (Math/floor (/ fct (math/expt p k )))))))

(defn dps [fct p]
  "dp: Power Series for p^k, 'p' in 'fct', where fct is in factorial form, i.e., fct -> 100 means 100!"
  (for [k (range 1 (inc (math/floor (/ (Math/log fct) (Math/log p)))))]
    (bigint (Math/floor (/ fct (math/expt p k ))))))

(defn not-divisible-by-five [upto]
  "Return a list excluding all factors of five from 1 to (and including) 'upto'"
  (filter #(not= 0 (mod % 5))(range 1N (inc upto))))

(defn M [upto sig]
  "Return last 'sig' non-zero digits from multiplying each factor not divisible by 5 from 1 to 'upto'"
  (last-sig-digits (reduce * (not-divisible-by-five upto)) sig))

(defn N [upto sig]
  "Returns {:N n, :twos twos}, where :twos is 2^2 per 10 digits in M[upto, sig]
                                     :N is M[upto, sig]/2^:twos"
  (let [twos (* 2N (quot upto 10))
        pow (discrete/power 10 sig)
         N  (last-sig-digits (/ (reduce * (not-divisible-by-five upto)) (discrete/power 2 twos)) sig)]
    (hash-map :N N, :twos twos)))

;; M_100 = N_100 * 2^20  is 22176 * 2^20
;; M_10  = N_10  * 2^2   is 18144 * 2^2
;; M_64  = N_64  * 2^12  is 30144 * 2^12
;; M_20  = N_20  * 2^4   is   736 * 2^4
;; M_12  = N_10  * 2^2   is 95008 * 2^2

(defn DP [fct base sig]
  (let [sig-10s (discrete/power 10 sig)
        twos (- (dp fct 5)
              (reduce + (map #(* 2 (quot % 10)) (conj (dps fct 5) fct))))
        N_base (:N (N base sig))
        exp (reduce + (map #(quot % base)(conj (dps fct 5) fct)))
        exp-prod (discrete/power-mod N_base exp sig-10s)
        rem-list (map #(mod % base)(conj (dps fct 5) fct))
        rem-prod (mod (reduce * (map #(:N (N % 5)) rem-list)) sig-10s)]
    (mod (/ (* exp-prod rem-prod) (discrete/power 2 twos)) sig-10s)))

(def euler160 (time (DP 1000000000000 10000 5)))

;;; euler160 -> 16576N  "Elapsed time: 164.135 msecs"
