#!/usr/bin/env cake
;;
;; euler39.clj
;;
;;

(use '[cbd])
(use 'clojure.contrib.math)
(use '[sage])

;; If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
;; {20,48,52}, {24,45,51}, {30,40,50}
;; For which value of p < 1000, is the number of solutions maximised?
;;
;; Answer: [840 8] ([840 [252 240 348]] [840 [280 210 350]] [840 [315 168 357]] [840 [336 140 364]] [840 [350 120 370]] [840 [360 105 375]] [840 [390 56 394]] [840 [399 40 401]])

(defn euler-39 []
  (let [find-z (fn [x y] (exact-integer-sqrt (+ (* x x) (* y y)))),
        triples (for [x (range 1 1000), y (range 1 x)
                      :when (let [[z z-rem] (find-z x y)]
                              (and (= 0 z-rem) (< (+ x y z) 1001)))]
                  (let [z (first (find-z x y))]
                    [(+ x y z) [x y z]])),
        max-triples (last (sort-by last (frequencies (map first triples))))]
    (for [t triples :when (= (first max-triples) (first t))] t)))

