;;
;; euler82.clj
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
    (cond
     (= (mod v cols) (dec cols) ) []                                    ; Last column -> no edges -> done
     :else
     (remove #(or (< % 0) (> % (dec (* rows cols))))                    ; Remove out of bound edges
             [(+ v 1) (+ v cols) (- v cols)])))                         ; Valid edges are Right, Down or Up

  (partition 2
             (flatten
              (remove (fn [[v, edges]] (empty? edges))                  ; If no edges, remove [v []]
                      (for [v (range (* rows cols))]                   ; [v <- {edges}]
                        (map #(vector v %) (f-edge-rules v)))))))      ; [v (edges0] - > [[v e1] [v e2] ...]

(defn adj-map
  "Returns a map keyed by vertex with a vector containing each adjacent vertex.
   - Takes a seq of edges of form [u v]
   - For example: (adj-map (edges 2 3)) -> {0 (1 3), 1 (2 0 4), 2 (1 5), 3 (4 0), 4 (5 3 1)}"
  [edges]
  (let [max-v (apply max (map second edges))]
    (apply hash-map
     (apply concat
            (map (fn [[k v]] [k (remove #{k} (flatten v))] )
                 (group-by #(mod (first %) max-v) edges))))))


(defn bellman-ford
  "Finds shortest path from source-vertex all other vertices in graph.
   -  edges is a seq of directed edge names [u v], where u->v
   -  weights is seq of weights for each v, s.t. weight[u v] = weight [v]
   -  source-vertex is the name of the starting vertex
   -  Returns sp-map  with key: [:dist, :prior, :size, :source-vertex"
  [edges weights source-vertex]

  (let [inf 99999999
        size (count weights)
        weight (to-array weights)
        dist (to-array (repeat size inf))
        prior (to-array (repeat size -1))]
    (aset dist source-vertex 0)
    (dotimes [i (dec size)]
      (doseq [[u v] edges]
        (let [d_u+w_v (+ (aget dist u) (aget weight v)),
              d_v (mget dist v)]
          (if (< d_u+w_v d_v)
            (do (aset dist v d_u+w_v)
                (aset prior v u))))))
    {:dist (into [] dist),
     :prior (into [] prior),
     :size size,
     :source-vertex source-vertex
     :weight weight}))


(defn dijkstra
  "Finds shortest path from source-vertex all other vertices in graph.
   -  edges is a seq of directed edge names [u v], where u->v
   -  weights is seq of weights for each v, s.t. weight[u v] = weight [v]
   -  source-vertex is the name of the starting vertex
   -  Returns sp-map  with key: [:dist, :prior, :size, :source-vertex"
  [edges weights source-vertex]

  (let [inf 9999999
        size (count weights)
        adj (adj-map edges)
        weight (to-array weights)
        prior (to-array (repeat size -1))
        dist (to-array (repeat size inf))
        queue (java.util.PriorityQueue. size  #(< (second %1) (second %2)))
        shortest (java.util.TreeSet. )]
    (doseq [v (range size)] (.add queue [v inf]))
    (.remove queue [source-vertex inf])
    (.add queue [source-vertex 0])
    (aset dist source-vertex 0)
    (loop []
      (when (not (.isEmpty queue))
        (let [[u, d_u]  (.poll queue)]
          (.add shortest [u, d_u])
          (doseq [v (adj u)]
            (let [d_u+w_v (+ d_u (aget weight v)),
                  d_v (aget dist v)]
              (if (< d_u+w_v d_v)
                (do (.remove queue [v d_v])
                    (.add queue [v d_u+w_v])
                    (aset dist v d_u+w_v)
                    (aset prior v u))))))
        (recur)))
    {:dist (into [] (map second (into (sorted-set) shortest))),
     :prior (into [] prior),
     :size size,
     :source-vertex source-vertex
     :weight weight}))


(defn path
  "Find shortest path from source to-vertex using prior.
   - Takes sp-map and to-vertex
  (interate (prior to-vertex) --> (... (prior (prior (prior to-vertex)))"
  [sp-map to-vertex]
  (let [prior (sp-map :prior)]
    (reverse (take-while #(>= % 0) (iterate prior to-vertex)))))


(defn matrix-form
  "Takes sp-map and key [:dist | :prior]"
  [sp-map key ]
  (mx/matrix (partition (int (Math/sqrt(sp-map :size)))  (sp-map key))))


(defn source-vertex->rhs
  "Takes sp-map
   - Returns path (source-vertex -> to-vertex) and total weight of path to-vertex
   - ToDo: Fix painful use of switching back and forth from matrix to vectors to get rhs-col"
  [sp-map rows cols]
  (let [source-vertex (sp-map :source-vertex),
        source-weight (nth (sp-map :weight) source-vertex) ;; In dist, source-vertex = 0 by def.
        ;; rhs-col (filter #(= (mod % cols) (dec cols)) (range (* rows cols)))
        rhs-col (mx/get-column (matrix-form sp-map :dist) (dec cols))]
    (let [to-vertex-weight (apply min rhs-col),
          to-vertex (dec (* cols (inc (.indexOf (into [] rhs-col) to-vertex-weight))))]
     [(path sp-map to-vertex) (+ source-weight to-vertex-weight )])))

(defn least-weight-lhs->rhs [weights rows cols]
  (let [es (edges rows cols)
        lhs-col (filter #(= (mod % cols) 0) (range (* rows cols)))]
    (last (first (sort-by second <
                     (for [sv lhs-col]
                       (source-vertex->rhs (dijkstra es weights sv) rows cols)))))))


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



;;;; Testing
(def vs-test
  (strings->vectors
   ["131,673,234,103,18"
    "201,96,342,965,150"
    "630,803,746,422,111"
    "537,699,497,121,956"
    "805,732,524,37,331"]))
(def test-weights  (flatten vs-test))
(defn test [] (least-weight-lhs->rhs test-weights 5 5))


;;;; Problem Data

(def e82-weights (flatten (strings->vectors (file->strings "src/euler82.txt"))))
(defn e82 [] (least-weight-lhs->rhs e82-weights  80 80))

;; Using Bellman-Ford: (e82) ->  260324 "Elapsed time: 5599120.023 msecs" (93 minutes)
;; Using Dijkstra:     (e82) ->  260324 "Elapsed time:   19167.992 msecs" (19 seconds)
