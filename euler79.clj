;;
;; euler79.clj
;;

(require '[cbd])
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
(require '[clojure.pprint :refer (pprint)])
(require '[clojure.set])
;(require '[clojure.data.priority-map :refer (priority-map)])
;(require '[clojure.core.matrix :as mx :refer (pm mget mset!)])
;(require '[clojure.core.matrix.operators :as m ])
;(mx/set-current-implementation :vectorz)
;(mx/set-current-implementation :ndarray)

;; A common security method used for online banking is to ask the user for three random
;; characters from a passcode. For example, if the passcode was 531278, they may ask


;; The text file, euler79.txt, contains fifty successful login attempts.

;; Give so as to determine the shortest possible secret passcode of unknown length.

(def codes-uniq
  (map cbd/integer->digits  (distinct (sort (map #(Long. %) (split-lines (slurp "src/euler79.txt")))))))

(def codes-digits (sort (distinct(flatten codes-uniq))))

(def codes-by-pos-uniq  (vector (sort (distinct(map first codes-uniq))) (sort (distinct (map second codes-uniq))) (sort (distinct (map last codes-uniq)))))


;;; Logic: Try to minimize number subject to constraints. Generally, try to place high digits to rhs of canidate  number

;; (pprint codes-uniq) ->
;; ((1 2 9)
;;  (1 6 0)
;;  (1 6 2)
;;  (1 6 8)
;;  (1 8 0)
;;  (2 8 9)
;;  (2 9 0)
;;  (3 1 6)
;;  (3 1 8)
;;  (3 1 9)
;;  (3 6 2)
;;  (3 6 8)
;;  (3 8 0)
;;  (3 8 9)
;;  (6 2 0)
;;  (6 2 9)
;;  (6 8 0)
;;  (6 8 9)
;;  (6 9 0)
;;  (7 1 0)
;;  (7 1 6)
;;  (7 1 8)
;;  (7 1 9)
;;  (7 2 0)
;;  (7 2 8)
;;  (7 2 9)
;;  (7 3 1)
;;  (7 3 6)
;;  (7 6 0)
;;  (7 6 2)
;;  (7 6 9)
;;  (7 9 0)
;;  (8 9 0))

;; codes-digits -> (1 2 3 6 7 8 9 0) -> Must be comprised of at least these 8 digits -> No 4's or 5's

;; codes-by-pos-uniq -> [(1 2 3 6 7 8) (1 2 3 6 8 9) (0 1 2 6 8 9)] -> [7 x x x x x x 0]

;; (8 9 0) -> [7 x x x x 8 9 0]

;; (7 3 1) -> [7 3 1 6 x 8 9 0] -> [7 3 1 6 2 8 9 0] -> 73162890
