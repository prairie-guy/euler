#!/usr/bin/env cake
;;
;; euler42.clj
;;
;;

(use '[cbd])
(require 'clojure.contrib.math)
(require 'clojure.string)

;; The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;;
;; By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we
;; form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number
;; then we shall call the word a triangle word.
;; Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand
;; common English words, how many are triangle words?
;;
;; Answer = 162



(defn pos [char] (- (int char) 64))
(defn word-value [word] (reduce + (map pos word)))
(def tri-numbers (map first (iterate (fn [[t_1 indx]] [(+ t_1 indx) (inc indx)]) [1 2])))
(defn tri-number? [n] (some #(= n %) (take-while #(<= % n) tri-numbers)))

(def dict (clojure.string/split (clojure.string/replace tw #"\"" "") #","))
(defn eulter-42 []
  (count (filter #(tri-number? (word-value %)) dict)))
