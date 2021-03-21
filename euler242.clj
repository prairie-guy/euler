(use 'tupelo.core)
(use 'cbd)
(require '[clojure.math.numeric-tower :as cmath])
(require '[clojure.math.combinatorics :as comb])

;; Euler 243 - Resilience

;; A positive fraction whose numerator is less than its denominator is
;; called a proper fraction.For any denominator, d, there will be d−1
;; proper fractions; for example, with d = 12:
;; 1/12 , 2/12 , 3/12 , 4/12 , 5/12 , 6/12 ,
;; 7/12 , 8/12 , 9/12 , 10/12 , 11/12 .

;; We shall call a fraction that cannot be cancelled down
;; a resilient fraction. Furthermore we shall define the resilience
;; of a denominator, R(d), to be the ratio of its proper fractions
;; that are resilient; for example, R(12) = 4/11 .
;; In fact, d = 12 is the smallest denominator having a
;; resilience R(d) < 4/10 .

;; Find the smallest denominator d, having a resilience
;; R(d) < 15499/94744 (/ 15499 94744)
;;

(defn R_orig [d]
  "For d = 6: R[6] -> {1/6,2/6,3/6,4/6/5/6} -> {1/6,5/6} -> 2 -> 2/5"
  (it-> (for [k (range 1 d)] (/ k d))
        (keep-if #(= d (denominator %)) it)
        (count it)
        (/ it (dec d))))

; Much faster than R_orig
(defn R [d] (/ (phi d) (dec d)))

(defn minimums []
  "The series R[d] is not monotonically decreasing.
   Minimums[] is the series [r,R[d]] for local minimums of R[d]
   {[d, R(d)]} -> {[6,2/5], [30,8/29], [210,48/209]}
   NOTE: This is used only to investigate the patterns generating
   only those R[d] that are local minima. R[d] are fractions.
   This is very inefficient as it loops through each d."
  (loop [d_0 1, r_min 1]
    (let [d (inc d_0)
          r (R d)
          ]
      (if (< r r_min)
      (do
        (prn [d, r])
        (recur d r))
      (recur d r_min)))))

(defn ds_mins []
  "The series of minimal d associated with minimal R[d].
   ds_min[k] is the product of the first k primes.
   ds_mins = {2,6,30,210...}"
  (for [k (map inc (range))] (primorial k)))

(defn numerator_mins []
  "Generates a series of the numerators for minimal R[d]
   based upon the recursion (The second is because primes is 0-based)
                   num[k] = (prime[k] -1)  * num[k-1]
   0-based primes: num[k] = (prime[k-1] -1)* num[k-1]
   numerator_mins = {1,2,8,48,480...}"
  (it-> (iterate (fn [[k num]]
                 [(inc k) (* num (dec (nth primes k)))])
                 [1N 1N])
        (map second it)))

(defn R_d_mins []
  (let [mins (ds_mins)
        nums (numerator_mins)
        dems (map dec mins)
        rs (map #(/ %1 %2) nums dems)]
    (map list mins rs)))


(defn minimal_d [limit]
  (let [res (last (take-while #(> (second %) limit) (R_d_mins)))
        d_0 (first res)
        num_0 (numerator (second res))]
    (it->(iterate (fn[[d num]]
                    [(+ d d_0) (+ num num_0) (dec (+ d d_0))])
                  [d_0 num_0 (dec d_0)])
         (drop-while #(> (/ (second %) (third %)) limit) it)
         (first it)
         (first it))))

(defn euler242 [] (minimal_d (/ 15499 94744)))
;; user> (time (euler242
;; "Elapsed time: 0.80012 msecs"
;; => 892371480



(defn euler242-alt [limit]
  (let [min (it-> (for [d (map primorial naturals)] (R d))
                  (take-while #(> % limit) it)
                  (last it))
        num_0 (numerator min)
        den_0 (denominator min)
        d_0   (inc den_0)]
    (prn [d_0 num_0 den_0])
    (it->(iterate (fn[[d num]]
                    [(+ d d_0) (+ num num_0) (dec (+ d d_0))])
                  [d_0 num_0 (dec d_0)])
         (drop-while #(> (/ (second %) (third %)) limit) it)
         (first it)
         (first it))
      ))


;; (minimums)
;* [2 1/1]
;; [4 2/3]
;* [6 2/5]
;; [12 4/11]
;; [18 6/17]
;; [24 8/23]
;* [30 8/29]
;; [60 16/59]
;; [90 24/89]
;; [120 32/119]
;; [150 40/149]
;; [180 48/179]
;* [210 48/209]
;; [420 96/419]
;; [630 144/629]
;; [840 192/839]
;; [1050 240/1049]
;; [1260 288/1259]
;; [1470 336/1469]
;; [1680 384/1679]
;; [1890 432/1889]
;; [2100 480/2099]
;* [2310 480/2309]
;; [4620 960/4619]
;; [6930 1440/6929]
;; [9240 1920/9239]
;; [11550 2400/11549]
;; [13860 2880/13859]
;; [16170 3360/16169]
;; [18480 3840/18479]
;; [20790 4320/20789]
;; [23100 4800/23099]
;; [25410 5280/25409]
;; [27720 5760/27719]
;* [30030 5760/30029]
;; [60060 11520/60059]
;; [90090 17280/90089]
;; [120120 23040/120119]
;; [150150 28800/150149]
;; [180180 34560/180179]
;; [210210 40320/210209]
;; [240240 46080/240239]
;; [270270 51840/270269]
;; [300300 57600/300299]
;; [330330 63360/330329]
;; [360360 69120/360359]
;; [390390 74880/390389]
;; [420420 80640/420419]
;; [450450 86400/450449]
;; [480480 92160/480479]
;* [510510 92160/510509]
