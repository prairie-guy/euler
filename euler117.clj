(use 'tupelo.core)
(require '[cbd])
(require '[clojure.math.numeric-tower :as cmath :refer [ceil floor]])
(require '[clojure.math.combinatorics :as comb])


;;
;; euler117 - Red, green or blue tiles
;;
;; Using a combination of grey square tiles and oblong tiles chosen from:
;; red tiles (2 units), green tiles (3 units), and blue tiles (4 units),
;; it is possible to tile a row measuring five units in length
;; in exactly fifteen different ways.
;;
;; How many ways can a row measuring fifty units in length be tiled?
;;


;;
;; if m is None or m >= n: yield [n]
;;   for f in range(n-1 if (m is None or m >= n) else m, 0, -1):
;;     for p in partitions(n-f, f): yield [f] + p
;;
;; WORK IN PROCESS DOES NOT WORK. TRYING tupelo.core.yield
(defn part-yield [n & {:keys [m] :or {m 0}}]
  (lazy-gen
   (if (or (zero? m) (>= m n))
     (yield [n])
     (doseq [f (range (if (or (zero? m) (>= m n)) (dec n) m) 0 -1)]
         (doseq [p (part-yield (- n f))]
           (yield-all (concat [f] p)))))))


;; (defn partitions-lexical [n]
;;   "Returns the partitions of the integer n"
;;   (if (zero? n)
;;     '(())
;;     (apply concat
;;       (for [p (partitions-lexical (dec n))]
;;         (let [res [(cons 1 p)]]
;;           (if (and (not (empty? p))
;;                    (or (< (count p) 2) (> (second p) (first p))))
;;             (conj res (cons (inc (first p)) (rest p)))
;;             res))))))

;; Find the partitions without sizes > 4 and count permutations
(defn multi-color [n]
  (reduce + (map comb/count-permutations
                 (keep-if (fn [p] (has-none? #(> % 4) p)) (cbd/partitions-lexical n)))))

;; Easier to read version
(defn multi-color-2 [n]
  (->> (cbd/partitions-lexical n)
       (keep-if (fn [p] (has-none? #(> % 4) p)))
       (map comb/count-permutations)
       (reduce +)))



;; (user>) (time(multi-color 50))
;; "Elapsed time: 1244.850736 msecs"
;; => 100808458960497N
