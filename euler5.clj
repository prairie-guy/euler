#!/usr/bin/env cake
;;
;; euler5.clj
;; What is the smallest number divisible by each of the numbers 1 to 20?
;;


(require '[cbd])
(use 'clojure.contrib.math)

(defn run []
 (->> (range 1 21)
     (map (comp set cbd/factors))
     (reduce clojure.set/union)
     (reduce *)))

(defn run2 []
  (reduce lcm (range 1 21)))

(prn (run2))




