#!/usr/bin/env cake
;;
;; euler60.clj
;;
;;

;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them
;; in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime.
;; The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
;; Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.


;(use 'sage)
(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

(def & comp)
(def p partial)

(def MAX-PRIME 10000)

(defn- commutable-prime-helper? [p q]
  (and (cbd/prime? (Integer. (join [(str p) (str q)] )))
       (cbd/prime? (Integer. (join [(str q) (str p)] )))))

(def ps (cbd/primes-up-to MAX-PRIME))  ;;  (2 3 5 7 11 ...)

(def rp (map vector ps))               ;;  ([2] [3] [5] [7] [11]...)

(def ps-map (into {}
                  (for [p ps] [p, (take-last (- (count ps) (.indexOf ps p) 1) ps) ])))
(def cp-map (into {}
                  (for [p ps, q ps] [[p q] (commutable-prime-helper? p q)])))


(defn commutable-prime? [p q]
  (cp-map [p q]))

(defn expand-a-ring [primes ring]
  (for [p primes :when (every? #(commutable-prime? p %) ring)]
    (conj ring p)))

(defn expand-all-rings [primes rings]
  (apply concat
         (for [ring rings
               :let [ex-ring (expand-a-ring (ps-map (last ring)) ring)]
               :when (not-empty ex-ring)]
           ex-ring)))

(defn euler-60 []
  (->> (expand-all-rings ps rp)
       (expand-all-rings ps)
       (expand-all-rings ps)
       (expand-all-rings ps)
       ))
