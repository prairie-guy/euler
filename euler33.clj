#!/usr/bin/env cake
;;
;; euler33.clj
;;

(use '[cbd])
(use 'clojure.contrib.math)


;; The fraction 49/98 is a curious fraction. It may incorrectly believed that 49/98 = 4/8,
;; which is correct, is obtained by cancelling the 9s.
;;
;; Consider fractions like, 30/50 = 3/5, to be trivial examples.
;;
;; There are exactly four non-trivial examples of this type of fraction, less than one in value,
;; and containing two digits in the numerator and denominator.
;;
;; If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
;;
;; (1/4 1/5 2/5 1/2)
;; 
;; Answer = 100


(defn curious-reduce [[num den]]
  (let [num (integer->list num), den (integer->list den)]
      (assert (= (count num) (count den)))
      (loop [n num, num-red num, den-red den]
        (cond (empty? num-red) [1 1]
              (empty? n) [(list->integer num-red) (list->integer den-red)]
              (some #(= (first n) %) den-red) (recur (rest n) (remove-n 1 #(= (first n) %) num-red) (remove-n 1 #(= (first n) %) den-red))
              :else (recur (rest n) num-red den-red)))))

(defn euler33 []
  (denominator (reduce *
  (for [n (range 10 100), d (range 10 100)
;;  (for [n (range 100 1000), d (range 100 1000)
        :when (let [[num-red den-red] (curious-reduce [n d])]
                (and
                 (not= 0 (mod n 10))
                 (not= 0 (mod d 10))
                 (not= n num-red)
                 (not= 0 den-red)
                 (not= 1 (/ n d))
                 (<  (/ n d) 1)
                 (= (/ n d) (/ num-red den-red))))]
    (/ n d)))))
    ;;    [ [n d] (/ n d)]))
    


                                               
        
                                                          
    




                