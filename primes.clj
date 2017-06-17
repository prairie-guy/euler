#!/usr/bin/env cake
;;
;; 

(use 'clojure.contrib.math)

;;(require 'clojure.contrib.lazy-seqs)
;;(defn prime-lazy [n-primes]
;;  (let [primes clojure.contrib.lazy-seqs/primes]
;;    (take n-primes primes)))


;; Not mine, but best performin.
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

;; As above, but uses ints.
(defn primes-t3 [size]
  (let [v (transient [])
        primes (transient [2])]
    (doseq [n (range size)] (assoc! v n 1))
    (doseq [n (range 3 (inc (int (sqrt size))) 2)]
      (if (== (v n) 1)
        (doseq [p (range (* n n) size n)]
          (assoc! v p 0))))
    (doseq [n (range 3 size 2)]
      (if (== (v n) 1)
        (conj! primes n)))
    (persistent! primes)))

;; No optimization for getting rid of evens.
(defn primes-orig [size]
  (let [v (transient [])
        primes (transient [])]
    (doseq [n (range size)] (assoc! v n 1))
    (doseq [n (range 2 (inc (int (sqrt size))))]
      (if (== (v n) 1)
        (doseq [p (range (* n n) size n)]
          (assoc! v p 0))))
    (doseq [n (range 2 size)]
      (if (== (v n) 1)
        (conj! primes n)))
    (persistent! primes)))


;; Uses Java Library BitSets. Slow.
(defn primes-jv [size]
  (let [v (doto (java.util.BitSet. (dec size)) (.set 0 (dec size)))
        primes (transient [])]
    (doseq [n (range 2 (inc (int (sqrt size))))]
      (if (.get v n)
        (doseq [p (range (* n n) size n)]
          (.set v p false))))
    (doseq [n (range 2 size)]
      (if (.get v n)
        (conj! primes n)))
    (persistent! primes)))

;; Non-transient, boolean array using aget/get. Slow
(defn  primes-b [size]
  (let [v (make-array Boolean/TYPE size)
        primes (transient [])]
    (doseq [n (range size)] (aset-boolean v n true))
    (doseq [n (range 2 (inc (int (sqrt size))))]
      (if (get v n)
        (doseq [p (range (* n n) size n)]
          (aset-boolean v p false))))
    (doseq [n (range 2 size)]
      (if (get v n)
        (conj! primes n)))
    (persistent! primes)))

;; Non-transient, int array using aget/get. Slow.
(defn primes-i [size]
  (let [v (make-array Integer/TYPE size)
        primes (transient [])]
    (doseq [n (range size)] (aset-int v n 1))
    (doseq [n (range 2 (inc (int (sqrt size))))]
      (if (== (get v n) 1)
        (doseq [p (range (* n n) size n)]
          (aset-int v p 0))))
    (doseq [n (range 2 size)]
      (if (== (get v n) 1)
        (conj! primes n)))
    (persistent! primes)))


(def prime-gen
  (let [primes (atom [])]
    (for [n (iterate inc 2)
          :when (not-any? #(zero? (rem n %))
                          (filter #(<= % (Math/sqrt n))
                                  @primes))]
      (do (swap! primes conj n)
                       n))))

(def *size* (int 1e8))

(prn "primes")
(time (def p (primes-up-to *size*)))
(time (def p (primes-tb-3 *size*)))
(time (def p (primes-t3 *size*)))
(time (def p (primes-orig *size*)))
;;(time (def p (primes-jv *size*)))
;;(time (def p (primes-b *size*)))
;;(time (def p (primes-i *size*)))
