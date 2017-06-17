#!/usr/bin/env cake
;;
;; euler18.clj
;;
;; By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
;;
;;  3
;; 7 4
;; 2 4 6
;; 8 5 9 3
;;
;;That is, 3 + 7 + 4 + 9 = 23.
;;
;; Find the maximum total from top to bottom in euler67.txt.
;;
;; Answer = 7273

(require '[cbd])
(require 'clojure.string)  

(def raw-text (slurp "src/euler67.txt"))


(def parsed-text
  (clojure.string/split-lines
   (clojure.string/replace raw-text #"0(\d)" #(str (second %)))))

(def *grid*
  (into []
   (reverse (for [line parsed-text]
              (into [] (map read-string
                            (clojure.string/split line #" ")))))))

(defn find-max [grid]
  (loop [tri (rest grid), last-row (first grid), result []]
    (if (empty? tri) (last result)
        (let [row  (into [] (for [i (range (count (first tri)))]
                     (+ (get (first tri) i) (max (get last-row i) (get last-row (inc i))))))]
          (recur (rest tri) row (conj  result row))))))

(prn (find-max *grid*))