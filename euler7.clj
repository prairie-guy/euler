#!/usr/bin/env cake
;;
;; Euler 7
;; Find the 10001st prime.
;;
(use 'clojure.contrib.math)


;; Not mine, but best performing
(defn primes-up-to [n]
  (let [n (int n)]
    "Returns a list of all primes from 2 to n"
    (let [root (int (Math/round (Math/floor (Math/sqrt n))))]
      (loop [i (int 3)
             a (int-array n)
             result (transient [2])]
        (if (>= i n)
          (persistent! result)
          (recur (+ i (int 2))
                 (if (< i root)
                   (loop [arr a
                          inc (+ i i)
                          j (* i i)]
                     (if (>= j n)
                       arr
                       (recur (do (aset arr j (int 1)) arr)
                              inc
                              (+ j inc))))
                   a)
                 (if (zero? (aget a i))
                   (conj! result i)
                   result)))))))

;; My best perfomer. Uses transients, true/false  arrays and ignores evens.
(defn primes-tb-3 [size]
  (let [v (transient [])
        primes (transient [2])]
    (doseq [n (range size)] (assoc! v n true))
    (doseq [n (range 3 (inc (int (sqrt size))) 2)]      
      (if (= (v n) true)
        (doseq [p (range (* n n) size n)]
          (assoc! v p 0))))
    (doseq [n (range 3 size 2)]
      (if (= (v n) true)
        (conj! primes n)))
    (persistent! primes)))

(defn lazy-primes-cgrande []
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve n]
            (if-let [step (sieve n)]
              (-> sieve
                  (dissoc n)
                  (enqueue n step))
              (enqueue sieve n (+ n n))))
          (next-primes [sieve n]
            (if (sieve n)
              (recur (next-sieve sieve n) (+ n 2))
              (cons n (lazy-seq (next-primes (next-sieve sieve n) (+ n 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))
