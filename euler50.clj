;;
;; euler50.clj
;;

;; The prime 41, can be written as the sum of six consecutive primes:
;; 41 = 2 + 3 + 5 + 7 + 11 + 13
;;
;; This is the longest sum of consecutive primes that adds to a prime below one-hundred.
;;
;; The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
;; 
;; Which prime, below one-million, can be written as the sum of the most consecutive primes?


(use '[sage])
(use '[cbd])
(use 'clojure.repl)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])

(defn sum-sequencial-primes [max]
  (let [small-primes  (apply sorted-set (take-while #(< % max) (cbd/primes-up-to max)))
        small-prime?  (fn [n] (if (small-primes n) n nil))
        seq-prime-sum (fn [ps]                                    ;; ps is a seq of primes
                        (->> (reductions + ps)                    ;; Lazy seq of intermediate sums of seq of primes
                             (take-while #(< % max))              ;; Only take those sums < max 
                             (map vector (iterate inc 1))         ;; Index each sum, i.e., ([0 2] [1 5] [3 10]...)
                             (filter #(small-prime? (second %)))  ;; Filter sums that are prime
                             (last)))]                            ;; The last [len-of-prime-seq seq-prime-sum ] is the longest seq
                            
    (->> (iterate rest small-primes)  ;; Infinite set of list: ((2 3 5 7...) (3 5 7...) (5 7 ...) ... () () () ...)
         (take (count small-primes))  ;; Only take non-nil lists
         (map #(seq-prime-sum %))     ;; List of [len-of-prime-seq seq-prime-sum], i.e., ([4 31] [6 41] ...)
         (sort-by first)              ;; Sort smallest to largest by len-of-prime-seq (also the index)
         (last))))                    ;; The last has the longest len-of-prime-seq