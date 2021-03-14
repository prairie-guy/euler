(require '[cbd])
(require '[clojure.math.numeric-tower :as cmath])
(require '[clojure.math.combinatorics :as comb])
(require '[clojure.set])

;;
;; euler116 - Red, green or blue tiles
;;
;; A row of five grey square tiles is to have a number of its tiles
;; replaced with coloured oblong tiles chosen from red (length 2),
;; green (length 3), or blue (length 4).
;;
;; If red (2) tiles  are chosen there are exactly seven ways this can be done.
;;
;; If green (3) tiles are chosen there are three ways
;;
;; And if blue (4) tiles are chosen there are two ways.
;;
; Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of
; replacing the grey tiles in a row measuring five units in length.


(defn color [n c]
  (reduce + (map comb/count-permutations
        (for [k (range 1 (inc (quot n c)))]
          (concat (repeat k c) (repeat (- n (* k c)) 1))))))

(defn all-colors [n]
  (+ (color n 2) (color n 3) (color n 4)))

(time (all-colors 50))

;; user> (time (all-colors 50))
;; "Elapsed time: 7.45084 msecs"
;; => 20492570929N
