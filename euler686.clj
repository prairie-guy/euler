;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powers of Two
;;
;; 2^7 = 128. The next power of two whose leading digits are "12" is 80.
;;
;; Define p(L,n) to be the nth-smallest value of j such that the base 10
;; representation of 2^j begins with the digits of L.
;;
;; So p(12,1)=7 and p(12,2)=80.
;;
;; You are also given that p(123,45)=12710.
;;
;; 12710 p(123,678910).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 2^80                   -> 1208925819614629174706176N
;; 80*log10(2)            -> 24.082399653118497
;; 80*log10(2)            -> 10^24 * 10^.082399653118497
;; 10 ^ 0.082399653118497 -> 1.2089258196146322
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require '[clojure.math.numeric-tower :refer (floor expt)])

(defn leading? [digits n]
  "Are digits leading part of n?"
  (let [ds (floor (Math/log10 digits))
        lgn (* n (Math/log10 2))
        sgn (- lgn (floor lgn))]
    (== digits (floor (expt 10 (+ sgn ds))))))

;; Natural numbers
(def powers (iterate #(+ 1 %) 1))

(defn get-powers-of-two [digits]
  "Return a power-of-two series for digits of length n"
  (filter #(leading? digits %) powers))

(defn get-powers-of-two-diff [digits]
  "Return the sequential difference between terms of power-of-two series"
  (map #(- %1 %2)
       (rest (get-powers-of-two digits ))
       (get-powers-of-two digits)))

(defn next-term [digits ring n]
  "Return the next term after n in power-of-two series"
  (+ n (first
        (drop-while #(not (leading? digits (+ n %)))
                    ring))))

(defn get-ring [digits]
  "Return the 3 integers by which each power-of-two series differ"
  (sort (take 3 (distinct (get-powers-of-two-diff digits)))))

(defn p
  "Return p(digits, n), the nth-smallest value of j such that
   the base 10 representation of 2^n begins with the 'digits'"
  ([digits n]
   (let [ring (get-ring digits)
        start (first (get-powers-of-two digits))]
     (p digits ring start n)))

  ([digits ring start n]
   (nth (iterate #(next-term digits ring %) start) (dec n))))


(comment
user> (p 12 1)
=> 7
user> (p 12 2)
=> 80
user> (p 123 45)
=> 12710
user> (time (p 123 678910))
"Elapsed time: 219.355594 msecs"
=> 193060223
user> (time (p 123 67891011))
"Elapsed time: 175.644112 msecs"
=> 19306498355
user> (time (p 123 1000000))
"Elapsed time: 255.656128 msecs"
=> 284367207
user> (time(p 123 10000000))
"Elapsed time: 2494.761795 msecso"
=> 2843684973
user> (time(p 123 100000000))
"Elapsed time: 25568.758248 msecs"
=> 28438235340
user> (time(p 123456 123456))
"Elapsed time: 340.609035 msecs"
=> 32835474725
)
