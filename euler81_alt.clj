;;#!/bin/bash lein-exec
;;
;; euler81_alt.clj
;;

;; In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by only
;; moving to the right and down, is indicated in bold red and is equal to 2427.

;; Find the minimal path sum, in matrix.txt, a 31K text file containing a 80 by 80 matrix, from
;; the top left to the bottom right by only moving right and down.

(require '[cbd])
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
(require '[clojure.pprint :refer (pprint)])
;; (require '[clojure.set])



(defn file->strings
  "Takes a file and returns a sequence of strings, one string per line."
  [file]
  (clojure.string/split-lines
   (slurp file)))

(defn strings->vectors
  "Takes a sequence of strings and returns a sequence of vectors "
  ([strings] (strings->vectors strings #", *"))
  ([strings re-delimiter]
     (into []
           (for [line strings]
             (into [] (map read-string
                           (clojure.string/split line re-delimiter)))))))

(defn make-weights
  "Makes a java array from a sequence of vectors"
  [vectors] (to-array-2d vectors))

(defn init-least-weights
  "Takes a java array of weights and initializes a new array"
  [weights]
  (let [size (count weights)]
    (to-array-2d
     (repeat size (repeat size 0)))))


(defn find-least-weight
  "Takes arrays w and lw. Finds the least-weights and updates lw[i][j]"
  [w lw]
  (let [size (count w)]
    (aset lw 0 0 (aget w 0 0))                             ;; lw[0][0] = w[0][0]
    (doseq [i (range 1 size)]
      (aset lw i 0 (+ (aget w i 0) (aget lw (dec i) 0))))  ;; lw[i][0] = w[i][0] + lw[i-1][0]
    (doseq [j (range 1 size)]
      (aset lw 0 j (+ (aget w 0 j) (aget lw 0 (dec j)))))  ;; lw[0][j] = w[0][j] + lw[0][j-1]
    (doseq [i (range 1 size), j (range 1 size)
            :let [least (min (aget lw (dec i) j) (aget lw i (dec j)))] ]
      (aset lw i j (+ least (aget w i j))))                ;; lw[i][j] = w[i][j] + min(w[i-1][j],w[i][j-1]
    ))


(defn test []
  (def vs-test
    (strings->vectors
     ["131,673,234,103,18"
      "201,96,342,965,150"
      "630,803,746,422,111"
      "537,699,497,121,956"
      "805,732,524,37,331"]))
  (def w-test (make-weights vs-test))
  (def lw-test (init-least-weights w-test))
  (find-least-weight w-test lw-test)
  lw-test)


(defn euler81 []
  (def w (make-weights
          (strings->vectors (file->strings "src/euler81.txt"))))
  (def lw (init-least-weights w))
  (find-least-weight w lw)
  (aget lw (dec (count lw)) (dec (count lw))))


;; (euler81) -> 427337
