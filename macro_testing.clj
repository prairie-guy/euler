;; Euler Template
(use 'tupelo.core)
(require '[cbd])
(require '[clojure.math.numeric-tower :as cmath])
(require '[clojure.math.combinatorics :as comb])


(defmacro define-x-safely []
  (let [sym (gensym 'x)]
    `(do
       (def ~sym 2)
       (list ~sym))))

(defmacro define-x-hygienically []
  `(do
     (def x# 2)
     (list x#)))

(defmacro unless
  "Similar to if but negates the condition"
  [condition & forms]
  `(if (not ~condition)
     ~@forms))

(defmacro unsplice
        [& coll]
  `(do ~@coll))

(defmacro yes-no->boolean
  [val]
  `(let [b# (= ~val "yes")]
     b#))

(defmacro sm [k a b]
  `(let [k ~k]
     [~k ~a ~b]
     ;(reduce + (for [sym (range ~a (inc ~b))] sym))
     ))

(defn q[n]
  (if (= n 1) 1
      (reduce + (for [k (range 1 (inc n))] (* n (q k))))))

(defn p[n]
  (if (= n 1) 1
      (summation 1 n #(p %))))

(defn r[n]

  )
