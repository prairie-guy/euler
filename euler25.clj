#!/usr/bin/env cake
;;
;; euler25.clj
;;
;; The Fibonacci sequence is defined by the recurrence relation:

;; Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
;; Hence the first 12 terms will be:

;; F1 = 1
;; F2 = 1
;; F3 = 2
;; F4 = 3
;; F5 = 5
;; F6 = 8
;; F7 = 13
;; F8 = 21
;; F9 = 34
;; F10 = 55
;; F11 = 89
;; F12 = 144
;; The 12th term, F12, is the first term to contain three digits.
;;
;; What is the first term in the Fibonacci sequence to contain 1000 digits?


(def naturals (iterate inc 1))

(def fib-seq (map list naturals
                  (map first (iterate (fn [[n n_1]] [(+' n n_1) n])  [1N 0N]))))

(defn run-1[]
  (some (fn [[ith fib]] (if (== 1000 (count (str fib))) ith, false)) fib-seq))

(defn run-2 []
  (loop [n 1N, n_1 1N, ith 2]
    (if (== 1000 (count (str n))) ith
        (recur (+' n n_1) n (inc ith)))))
