#!/usr/bin/env cake
;;
;; euler16.clj
;;
;; 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;;
;; What is the sum of the digits of the number 2^1000?
;;
;; Answer = 1366

(use '[cbd])
(use 'clojure.contrib.math)

(defn run []  (reduce + (integer->list (expt (biginteger 2) 1000))))