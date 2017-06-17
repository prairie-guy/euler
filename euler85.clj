;;
;; euler85.clj
;;

(require '[cbd])
(require '[clojure.math.numeric-tower :as math :refer :all])
(require '[clojure.math.combinatorics :as combinatorics])
(require '[clojure.repl :refer :all])
(require '[clojure.string :refer (join trim split split-lines)])
(require '[clojure.pprint :refer (pprint)])
(require '[clojure.set])
(require '[clojure.data.priority-map :refer (priority-map)])
(require '[clojure.core.matrix :as mx :refer (pm mget mset!)])
;(require '[clojure.core.matrix.operators :as m ])
;(mx/set-current-implementation :vectorz)
;(mx/set-current-implementation :ndarray)

;; A rectangular grid measuring 3 by 2 contains eighteen rectangles
;; Although there exists no rectangular grid that contains exactly two million rectangles,
;; find the area of the grid with the nearest solution.

;; With Retangular Formula: N(m,n) = (1/4) m n (m+1) (n+1), where N is number of rectangles in a (n x m) rectangle
;;
;; Find {m', n'} s.t.  Abs (N(m',n') - N_goal) is minimized, where N_goal is the target N
;;
;; m =~ sqrt[ 4 N_goal /(n (n+1)]
;; For a given n, m_lower = floor [sqrt [ 4 N_goal /(n (n+1))]] - 1,
;; and            m_upper = ceil  [sqrt [ 4 N_goal /(n (n+1))]] + 1,
;;
;; Finally, n_max =~ N_goal^(1/4),

(defn N [m, n]  (* 1/4 m n (+ m 1)  (+ n 1)))

(defn M_lower [n,N_goal ] (floor (dec (sqrt (* 4 N_goal (/ 1 (* n (+ n 1))))))))
(defn M_upper [n,N_goal ] (ceil (inc (sqrt (* 4 N_goal (/ 1 (* n (+ n 1))))))))


(defn euler58-count-count [N_goal]
  (let [n_max (inc (ceil (Math/pow N_goal 1/4)))]
    (count    (sort-by last
                       (for [n (range 1 (inc n_max)) , m  (range (M_lower n N_goal ) (inc (M_upper n N_goal)))]
                         [n m (abs (- N_goal (N n m)))])))))

;; (euler58 2e6) - > 155

(defn euler [N_goal]
  (let [n_max (inc (ceil (Math/pow N_goal 1/4)))]
    (apply *
           (take 2
                 (first
                  (sort-by last
                           (for [n (range 1 (inc n_max)) ,
                                 m  (range (M_lower n N_goal ) (inc (M_upper n N_goal)))]
                             [n m (abs (- N_goal (N n m)))])))))))

;; (euler-58 2e6) -> 2772.0 "Elapsed time: 10.293 msecs"
