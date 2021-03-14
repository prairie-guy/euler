;;
;; euler100.clj
;;

(require  [cbd.clj])
(require '[clojure.math.numeric-tower :as cmath])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.string :refer (join trim split split-lines)])
(require '[clojure.repl :refer :all])
(require '[clojure.pprint :refer (pprint)])
;;(require '[clojure.set])
;;(require '[clojure.data.priority-map :refer (priority-map)])
;;(require '[clojure.core.matrix :as mx :refer (pm mget mset!)])
;;(require '[clojure.core.matrix.operators :as m ])
;;(mx/set-current-implementation :vectorz)
;;(mx/set-current-implementation :ndarray)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Box contains twenty-one coloured discs, composed of fifteen blue discs and six red discs, and two discs were
;; taken at random, it can be seen that the probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.
;;
;; The next such arrangement, for which there is exactly 50% chance of taking two blue discs at random, is a box
;; containing eighty-five blue discs and thirty-five red discs.
;;
;; By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in total, determine the number
;; of blue discs that the box would contain.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn blue_disks [container]
  (let [expr (+ (* 2N container container) (* -2 container) 1)
        sqrt (cmath/exact-integer-sqrt expr)
        sqrt (if (zero? (last sqrt)) (first sqrt) false)]
    (if  (and sqrt
              (integer? (/ (inc sqrt) 2)))
      (/ (inc sqrt) 2)
      false)))

(defn search-range [start stop]
  (for[n (range start (inc stop)) :when (blue_disks n) :let [b (blue_disks n) r (- n b)] ]
    [n b r]  ))


(defn search-range-factors [start stop]
  (for [[n b r] (search-range start stop)]
    [[(cbd/factors n) (cbd/factors (- n 1 ))] [(cbd/factors b) (cbd/factors (- b 1))]]))

 (defn search-from [from]
  (take 1 (for[n (range from (* 6.0 from)) :when (blue_disks n) :let [b (blue_disks n) r (- n b)] ]
     [n b r]  )))

(defn estimate []
  "Used to estimate the ratio of n[i+1]/n[i], which is multiplied by the final n found to get an estimate
   of the first n > 1e12 "
  (let [searches (search-range  800000 10000000)
        base-n (first (second searches))
        m (apply / (reverse (map first searches)))]
    (bigint (first (drop-while #(< % 1000000000000) (iterate #(* m %) base-n))))))


(defn euler100 [n]
  (time
   (search-from (estimate))))

;; (estimate) --> 1070375363450N

;; (euler100) "Elapsed time: 9572.061 msecs" -> ([1070379110497N 756872327473N 313506783024N])

;; 756872327473
