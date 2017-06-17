#!/usr/bin/env cake
;;
;; euler51.clj
;;


;; By replacing the 1st digit of *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
;;
;; By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example
;; having seven primes among the ten generated numbers, yielding the family:
;; 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family,
;; is the smallest prime with this property.
;;
;; Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit,
;; is part of an eight prime value family.


;(use 'sage)
(use 'clojure.repl)
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set]) 

(def expt clojure.math/expt)
(def join clojure.string/join)
(def trim clojure.string/trim)

(def subsets clojure.combinatorics/subsets)

(def SEP \space )

(defn make-prime-string [order]
  (str
   (join
    (interpose SEP
               (filter #(and (>= % (expt 10 (dec order))) (< % (expt 10 order)))
                       (cbd/primes-up-to (expt 10 order)))))
   SEP))

(defn- make-digital-masks [order]
  "Order is # digits in prime. Return vec of (2^ order - 2) subsets formed from [0, 1, 2, ... n], exlusive of null-set and full-set."
  (map vec (butlast (rest (subsets (range 1 (inc order)))))))

(defn- make-re-mask [digital-mask order]
  (re-pattern
   (str 
    (join 
     (if (= 1 (count digital-mask))
       (into [] (repeat order "(.)"))
       (apply (partial assoc (vec (repeat order "(.)")))
              (interleave (map dec (rest digital-mask)) (repeat (str "\\" (first digital-mask)))))))
    SEP)))

(defn- make-key-fn [digital-mask order]
  (let [parm (vec
               (clojure.set/difference
                (set (range 1 (+ 2 (- order (count digital-mask)))))
                (set (vector (first digital-mask)))))]
    (fn [match] (Integer. (join (map match parm))))))

(defn mask-struct [digital-mask order]
  "Create a struct {:mask re-mask, :key-fn make-key-fn}"
  {:mask (make-re-mask digital-mask order), :key-fn (make-key-fn digital-mask order)}
  )

(defn mask-match [m-struct prime-string]
  "Return vec [& [prime key]] where key is the prime masked according to mask-struct"
  (let [matches (re-seq (m-struct :mask) prime-string)]
    (for [m matches] [((m-struct :key-fn) m)  (Integer. (join (butlast (m 0))))])))

(defn mask-match-debug [m-struct prime-string]
  (let [matches (re-seq (m-struct :mask) prime-string)]
    [matches 
     (for [m matches] [((m-struct :key-fn) m)  (Integer. (join (butlast (m 0))))])]))

(defn longest-family [m-struct prime-string]
  "Return longest seq of primes that have been masked according to mask-struct"
  (let [masked-matches (mask-match m-struct prime-string)
        longest (first (last (sort-by second (frequencies (map first masked-matches)))))]
    (map second (filter #(= longest (first %)) masked-matches))))

(defn longest-family-by-order [order]
  (let [prime-string (make-prime-string order)
        digital-masks (make-digital-masks order)]
    (first
     (sort-by count > 
              (for [dm digital-masks] (longest-family (mask-struct dm order) prime-string))))))

(defn euler-51 [order]
  (first
   (sort-by count >
            (for [o (range 2 (inc order))] (longest-family-by-order o)))))

(print (euler-51 6))


;;;;; Testing against hand-wired mask-strut's 
(def two (make-prime-string 2))
(def three (make-prime-string 3))
(def four (make-prime-string 4))
(def five (make-prime-string 5))
(def six (make-prime-string 6))



;; Order 2: "11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 "
;; ([1] [2])

(def p_2_1 {:mask #"(.)(.) ", :key-fn (fn [match] (Integer. (join (map match [2]))))})
(def p_2_2 {:mask #"(.)(.) ", :key-fn (fn [match] (Integer. (join (map match [1]))))})

(defn test-two [] (map #(longest-family % two) [p_2_1 p_2_2]))

;; Order 3: "101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 961 967 971 977 983 991 997 "

;; ([1] [2] [3] [1 2] [1 3] [2 3])
(def p_3_1 {:mask #"(.)(.)(.) ", :key-fn (fn [match] (Integer. (join (map match [2 3]))))})       ;; [1 . . ]
(def p_3_2 {:mask #"(.)(.)(.) ", :key-fn (fn [match] (Integer. (join (map match [1 3]))))})       ;; [. 2 . ]
(def p_3_3 {:mask #"(.)(.)(.) ", :key-fn (fn [match] (Integer. (join (map match [1 2]))))})       ;; [. . 3 ]

(def p_3_12 {:mask #"(.)\1(.) ", :key-fn (fn [match] (Integer. (join (map match [2]))))})         ;; [1 1 . ]
(def p_3_13 {:mask #"(.)(.)\1 ", :key-fn (fn [match] (Integer. (join (map match [2]))))})         ;; [1 . 1 ]
(def p_3_23 {:mask #"(.)(.)\2 ", :key-fn (fn [match] (Integer. (join (map match [1]))))})         ;; [. 2 2 ]

(defn test-three [] (map #(longest-family % three) [p_3_1 p_3_2 p_3_3 p_3_12 p_3_13 p_3_23 ]))

;; Order 4
;; ([1] [2] [3] [4] [1 2] [1 3] [1 4] [2 3] [2 4] [3 4] [1 2 3] [1 2 4] [1 3 4] [2 3 4])
(def p_4_1 {:mask #"(.)(.)(.)(.) ", :key-fn (fn [match] (Integer. (join (map match [2 3 4]))))})  ;; [1 . . . ] 
(def p_4_2 {:mask #"(.)(.)(.)(.) ", :key-fn (fn [match] (Integer. (join (map match [1 3 4]))))})  ;; [. 2 . . ]
(def p_4_3 {:mask #"(.)(.)(.)(.) ", :key-fn (fn [match] (Integer. (join (map match [1 2 4]))))})  ;; [. . 3 . ]
(def p_4_4 {:mask #"(.)(.)(.)(.) ", :key-fn (fn [match] (Integer. (join (map match [1 2 3]))))})  ;; [. . . 4 ]

(def p_4_12 {:mask #"(.)\1(.)(.) ", :key-fn (fn [match] (Integer. (join (map match [2 3]))))})    ;; [1 1 . . ]
(def p_4_13 {:mask #"(.)(.)\1(.) ", :key-fn (fn [match] (Integer. (join (map match [2 3]))))})    ;; [1 . 1 . ]
(def p_4_14 {:mask #"(.)(.)(.)\1 ", :key-fn (fn [match] (Integer. (join (map match [2 3]))))})    ;; [1 . . 1 ]
(def p_4_23 {:mask #"(.)(.)\2(.) ", :key-fn (fn [match] (Integer. (join (map match [1 3]))))})    ;; [. 2 2 . ]
(def p_4_24 {:mask #"(.)(.)(.)\2 ", :key-fn (fn [match] (Integer. (join (map match [1 3]))))})    ;; [. 2 . 2 ]
(def p_4_34 {:mask #"(.)(.)(.)\3 ", :key-fn (fn [match] (Integer. (join (map match [1 2]))))})    ;; [. . 3 3 ]

(def p_4_123 {:mask #"(.)\1\1(.) ", :key-fn (fn [match] (Integer. (join (map match [2]))))})      ;; [1 1 1 . ] 
(def p_4_124 {:mask #"(.)\1(.)\1 ", :key-fn (fn [match] (Integer. (join (map match [2]))))})      ;; [1 1 . 1 ]
(def p_4_134 {:mask #"(.)(.)\1\1 ", :key-fn (fn [match] (Integer. (join (map match [2]))))})      ;; [1 . 1 1 ]
(def p_4_234 {:mask #"(.)(.)\2\2 ", :key-fn (fn [match] (Integer. (join (map match [1]))))})      ;; [. 2 2 2 ]

(defn test-four []
  (map #(longest-family % four) [p_4_1 p_4_2 p_4_3 p_4_4 p_4_12 p_4_13 p_4_14 p_4_23 p_4_24 p_4_34 p_4_123 p_4_124 p_4_134 p_4_234]))

