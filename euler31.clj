#!/usr/bin/env cake
;;
;; euler31.clj
;;

(use '[cbd])
(use 'clojure.contrib.math)
;;
;; How many different ways can 200 be made using any number of these coins?
;; 1, 2, 5, 10, 20, 50, 100, 200.

;; For example, it is possible to make 200 in the following way:
;;
;; 1x100 + 1X50p + 2X20p + 1x5p + 1x2p + 3x1p


(defn cc [amount coins] 
  (cond (= amount 0) 1
        (or (< amount 0) (empty? coins)) 0
        :else (+ (cc (- amount (first coins)) coins)
                 (cc amount (rest coins)))))

(prn (cc 200 [200, 100, 50, 20, 10, 5, 2, 1]))