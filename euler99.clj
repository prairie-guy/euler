;;
;; euler99.clj
;;

(require '[cbd])
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
;;
;; Comparing two numbers written in index form like 2^11 and 3^7 is not difficult, as any calculator would
;; confirm that 2^11 = 2048 < 3^7 = 2187. However, confirming that 632382^518061 > 519432^525806 would be
;; much more difficult, as both numbers contain over three million digits.
;;
;; Using euler99.txt  22K text file containing one thousand lines with a base/exponent pair on each line,
;; determine which line number has the greatest numerical value.
;;
;; NOTE: The first two lines in the file represent the numbers in the example given above.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn euler99 []
  (time
   (->> (slurp "src/euler99.txt")
        (split-lines)
        (map #(split %, #","))
        (map (fn [[b,e]]  [(Integer. b), (Integer. e)]))
        (map (fn [[b,e]] (* e (Math/log b))))
        (map vector (rest (range)))
        (sort-by second)
        (last)
        (first))))

;; (euler99) "Elapsed time: 16.509 msecs" -> 709


;;28433 ×2^ 7830457 +1
