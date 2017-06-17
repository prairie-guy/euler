#!/usr/bin/env cake
;;
;; euler20.clj

;; Find the sum of the digits in the number 100!
;;
;; Answer = 648

(use '[cbd])
(use 'clojure.contrib.math)


(defn run [] (reduce + (cbd/integer->list (fact 100))))