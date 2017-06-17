#!/usr/bin/env cake
;;
;; haskell.clj
;;
;; Using 'match' for haskell style programming
;;

(use '[cbd])
(use 'clojure.contrib.math)

(defn zip [l-1 l-2]
  (match [l-1 l-2]
         [_ []] []
         [[] _] []
         [[x & xs] [y & ys]] (cons [x y]  (zip xs ys))))


(defnp my_fact      ;; Stack overflow for large x
  0 1
  x (* x (my_fact (dec x))))

(defn my_fact_alt [n]
  (loop [i 1, result 1]
    (match [i result]
           [i _]    :when (= i n) (* i result)
           _    (recur (inc i) (* i result)))))

(defn my_map [f lst]
  (match [f lst]
         [_ [] ]      []
         [f [x & xs]] (cons (f x) (my_map f xs))))


;filter _pred []    = []
;filter pred (x:xs)
;| pred x         = x : filter pred xs
;| otherwise      = filter pred xs

(defn my_filter [pred lst]
  (match [pred lst]
         [_ [] ]         []
         [_ [x & xs]]    :when (pred x) (cons x (my_filter pred xs))
         _               (my_filter pred xs)))

(defn ++ [l1 l2]
    (match [l1 l2]
           [ [] ys]          ys
           [[x & xs] ys]     (cons x (++ xs ys))))
  
