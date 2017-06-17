;;#!/bin/bash lein-exec
;;
;; euler81.clj
;;

;; In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by only
;; moving to the right and down, is indicated in bold red and is equal to 2427.

;; Find the minimal path sum, in matrix.txt, a 31K text file containing a 80 by 80 matrix, from
;; the top left to the bottom right by only moving right and down.

(use 'clojure.repl)
;;(use '[clojure.string :only (join trim split split-lines)])
(require 'clojure.string)
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])
(require 'sage)

(defn file->strings [file]
  (clojure.string/split-lines
   (slurp file)
   ))

(defn strings->vectors
  ([strings] (strings->vectors strings #", *"))
  ([strings re-delimiter]
  (into []
   (for [line strings]
     (into [] (map read-string
                   (clojure.string/split line re-delimiter)))))))

(defn aconj-in [array idx val]
  (aset array idx (conj (aget array idx) val)))

(defn triangularize [vectors]
  (let [dim (count vectors)
        atr (to-array (repeat (+ 1 (* 2 (dec dim))) []))]
    (doseq [y (range dim), x (range dim)]
      (aconj-in atr (+ y x) (get-in vectors [x y])))
    ;; Example: dim=2 -> ;; [[[0 0]] [[0 1][1 0]] [[0 2] [1 1] [2 0]]]
    ;; atr
    (into [] atr)))

(defn dynamic-reduce
  ([from with] (dynamic-reduce from with max))
  ([from with cmp]
     (let [from-size (count from), with-size (count with)]
       (into []
             (cond
              (< from-size with-size)
              (for [w (range with-size)]
                (let [diff (- w (dec from-size))]
                  (cond
                   (= w 0)    (+ (with 0) (from 0) )
                   (= diff 1) (+ (with w) (from (dec w)) )
                   (> diff 1) (with w)
                   :else      (+ (with w) (cmp (from w) (from (dec w)))))))
              (> from-size with-size)
              (for [w (range with-size)]
                (+ (with w) (cmp (from w) (from (inc w)))))
              ;; :else <???>
              )))))

(def vs
  (strings->vectors
   ["131,673,234,103,18"
    "201,96,342,965,150"
    "630,803,746,422,111"
    "537,699,497,121,956"
    "805,732,524,37,331"]))

(def vt (triangularize vs))
;; [[131]
;;  [201 673]
;;  [630 96 234]
;;  [537 803 342 103]
;;  [805 699 746 965 18]
;;  [732 497 422 150]
;;  [524 121 111]
;;  [37 956]
;;  [331]]

(defn euler81 []
  (reduce #(dynamic-reduce %1 %2 min)
          (triangularize (strings->vectors (file->strings "src/euler81.txt")))))

 ;;(euler81) -> 427337



