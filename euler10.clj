#!/usr/bin/env cake
;;
;; euler10.clj
;;
;; Find the sum of all the primes below two million.
;;
;; Answer = 142913828922

(use '[cbd])

(prn (reduce + (primes-up-to 2e6)))