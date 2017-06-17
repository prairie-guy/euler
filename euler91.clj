;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; euler91.clj
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
;; The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and are joined to the origin,
;; O(0,0), to form ΔOPQ.
;;
;; There are exactly fourteen triangles containing a right angle that can be formed when each co-ordinate lies
;; between 0 and 2 inclusive; that is, 0 ≤ x1, y1, x2, y2 ≤ 2.
;;
;; Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn contains-right-angle? [[a b] [d e]]
  (let [inner (+ (* a d) (* b e))]
    (cond
      (= [a b] [0 0]) false                  ; No zero vectors
      (= [d e] [0 0]) false                 
      (= [a b] [d e]) false                  ; Vectors need to be distinct
      (and (zero? b) (zero? d))     true     ; Right Triangle at Angle 0 (The Orgin)
      (= inner (+ (* a a) (* b b))) true     ; Right Triangle at Angle 1
      (= inner (+ (* d d) (* e e))) true     ; Right Triangle at Angle 1
      :else false)))

(defn right-triangle-in-a-grid [n]
  "Sort each pair of vectors by x then y and then eliminate duplicates"
  (distinct (for [a (range (inc n)), b (range (inc n)), d (range (inc n)), e (range (inc n))
         :when (contains-right-angle? [a b] [d e])]
              (sort-by (juxt first second) [[a b] [d e]]))))

(def r (count (right-triangle-in-a-grid 2)))

;; r -> 14

(defn euler91 [] (time (count (right-triangle-in-a-grid 50))))

;; (euler91) -> 
;; "Elapsed time: 3611.487 msecs"
;; 14234



