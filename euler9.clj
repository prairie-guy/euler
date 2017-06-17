o#!/usr/bin/env cake
;;
;; euler8.clj
;;
;; Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.
;;
;;
;; Answer: [200^2 +  375^2 =  425^2 = 180625 ]
;;
;;         [200 *  375 *  425 = 31875000])

(require '[cbd])

(prn (for [i (range 1 1001) j (range i 1001) k (range j 1001)
           :when (and (== (* k k)  (+ (* i i) (* j j))) (== 1000 (+ i j k)))]
       [ i j k (* i j k)]))



