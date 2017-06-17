;;
;; euler83_bellford.clj
;;


;; In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right,
;; by moving left, right, up, and down, is indicated in bold red and is equal to 2297.

;; Find the minimal path sum, in matrix.txt, a 31K text file containing a 80 by 80 matrix, from
;; the top left to the bottom right by only moving right and down.

(require '[cbd])
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
(require '[clojure.pprint :refer (pprint)])
(require '[clojure.set])
(require '[clojure.core.matrix :as mx :refer (pm mget mset!)])
;(require '[clojure.core.matrix.operators :as m ])
;(mx/set-current-implementation :vectorz)
;(mx/set-current-implementation :ndarray)


(defn edges
  "Returns sequence of all edges in a matrix, each in form [u v]
   - Applies f-edge-rules on each vetrex of a matrix (rows x cols)
   - Vertices {0, 1, ... [rows x cols - 1]} Note: 0 is first element
   - REM: Vertices must be Integers starting with 0
   - Change f-edge-rules for different edge"
  [rows cols]
  (defn- f-edge-rules [v]
    (remove #(or (< % 0) (> % (dec (* rows cols))))                       ; Remove out of bound edges
            (cond
             (= v 0) [(+ v 1) (+ v cols)]                                 ; First v
             (= v (dec (* rows cols))) []                                 ; Last v
             (= (mod v cols) 0) [ (+ v 1) (+ v cols) (- v cols)]          ; First column
             (= (mod v cols) (dec cols)) [ (- v 1) (+ v cols) (- v cols)] ; Last column
             :else
             [(+ v 1) (- v 1) (+ v cols) (- v cols)])) )                  ; Valid edges are Right, Down or Up

  (partition 2
             (flatten
              (remove (fn [[v, edges]] (empty? edges))                   ; If no edges, remove [v []]
                      (for [v (range (* rows cols))]                    ; [v <- {edges}]
                        (map #(vector v %) (f-edge-rules v)))))))       ; [v (edges0] - > [[v e1] [v e2] ...]

(defn bellman-ford
  "Finds shortest path from source-vertex all other vertices in graph.
   -  edges is a seq of directed edge names [u v], where u->v
   -  weights is seq of weights for each v, s.t. weight[u v] = weight [v]
   -  source-vertex is the name of the starting vertex
   -  Returns bf-map  with key: [:dist, :prior, :size, :source-vertex"
  [edges weights source-vertex]

  (let [inf 9999999
        size (count weights)
        weight (to-array weights)
        dist (to-array (repeat size inf))
        prior (to-array (repeat size -1))]
;;    (aset dist source-vertex 0)
    (aset dist source-vertex (aget weight source-vertex))
    (dotimes [i (dec size)]
      (doseq [[u v] edges]
        (let [d_u+w_v (+ (aget dist u) (aget weight v)),
              d_v (aget dist v)]
          (if (< d_u+w_v d_v)
            (do (aset dist v d_u+w_v)
                (aset prior v u))))))
      (doseq [[u v] edges]
        (let [d_u+w_v (+ (aget dist u) (aget weight v)),
              d_v (aget dist v)]
          (if (< d_u+w_v d_v)
            (print "error: Graph contains a negative-weight cycle"))))
    {:dist (into [] dist),
     :prior (into [] prior),
     :size size,
     :source-vertex source-vertex
     :weight weight}))


(defn path
  "Find shortest path from source to-vertex using prior.
   - Takes bf-map and to-vertex
  (interate (prior to-vertex) --> (... (prior (prior (prior to-vertex)))"
  [bf-map to-vertex]
  (let [prior (bf-map :prior)]
    (reverse (take-while #(>= % 0) (iterate prior to-vertex)))))


(defn matrix-form
  "Takes bf-map and key [:dist | :prior]"
  [bf-map key ]
  (mx/matrix (partition (int (Math/sqrt(bf-map :size)))  (bf-map key))))


;;;; Utilities
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

;;;; Testing (Answer 2297)
(def vs-test
  (strings->vectors
   ["131,673,234,103,18"
    "201,96,342,965,150"
    "630,803,746,422,111"
    "537,699,497,121,956"
    "805,732,524,37,331"]))


(def test-weights  (flatten vs-test))
(defn test [] (bellman-ford (edges 5 5) test-weights 0))

;;;; Problem Data

(def e83-weights (flatten (strings->vectors (file->strings "src/euler83.txt"))))

(defn e83 [] (bellman-ford (edges 80 80) e83-weights 0))
