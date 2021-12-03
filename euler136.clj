;; Euler 136 -- Singleton difference
;;
;; The positive integers, x, y, and z, are consecutive terms of an
;; arithmetic progression. Given that n is a positive integer,
;; the equation, x2 − y2 − z2 = n, has exactly one solution when n = 20:
;;
;; 13^2 − 10^2 − 7^2 = 20
;;
;; In fact there are twenty-five values of n below one hundred for
;; which the equation has a unique solution. How many values of n
;; less than fifty million have exactly one solution?
;;
;; Define [k x n] as follows;
;; 13^2 −    10^2 −      7^2 = 20  => [3 13 20]
;;  x^2 - (x-k)^2 - (x-2k)^2 =  n  => [k  x  n]
;;  (x-k)(5k-x) = n

(use '[cbd])
(use 'tupelo.core)

(defn kx->n [k x] (+ (- (* x x)) (* 6 k x) (- (* 5 k k))))

(defn kx->factors [k x] [(- x k) (- (* 5 k) x) (kx->n k x)])

(defn kx->xyz [k x] [x (- x k) (- x k k)])

(defn k->ns [k]
  (sort < (for [x (range (inc (* 2 k)) (* 5 k))]
            (kx->n k x))))

(defn k->kxn [k]
  (->>(for [x (range (inc (* 2 k)) (* 5 k))]
        [k x (kx->n k x) ])))

(defn k->kxn-xyz-facts [k]
  (->>(for [x (range (inc (* 2 k)) (* 5 k))]
        [[k x (kx->n k x) ] (kx->xyz k x) (kx->factors k x)])))


(defn k->kxn-xyz-facts-distinct [k]
  (it->(for [x (range (* 4 k) (* 5 k))]
         [[k x (kx->n k x) ] (kx->xyz k x) (kx->factors k x)])
       (conj it [[k (* 3 k) (kx->n k (* 3 k)) ]
                 (kx->xyz k (* 3 k))
                 (kx->factors k (* 3 k))])))

(defn kmax->kxn [kmax]
  (->> (for [k (range 1 (inc kmax))] (k->kxn k))
       (apply concat)
       ;(sort-by #(nth % 2))
       ))

(defn kmax->kxn-xyz [kmax]
  (->> (for [k (range 1 (inc kmax))] (k->kxn-xyz k))
       (apply concat)
       (sort-by #(third (first %)))))

;; nmax = 4k^2
;; (upto-n n n/4) -> {n1,n2,...,n}
(defn upto-n [n k]
  (->> (kmax->kxn k)
       (map last)
       (sort)
       (partition-by identity)
       (filter #(= 1 (count %)))
       (map first)
       (filter #(<= % n))
       ))

(defn upto-n-2 [n k]
  (->> (for [j (range 1 k)] (k->kxn-xyz-facts-distinct j))
       (apply concat)
       (map last)
       (map last)
       (sort)
       (partition-by identity)
       (filter #(= 1 (count %)))
       (map first)
       (filter #(<= % n))
       ))
