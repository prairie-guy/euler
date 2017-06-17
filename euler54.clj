#!/usr/bin/env cake
;;
;; euler54.clj
;;

;; In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

;; High Card: Highest value card.
;; One Pair: Two cards of the same value.
;; Two Pairs: Two different pairs.
;; Three of a Kind: Three cards of the same value.
;; Straight: All cards are consecutive values.
;; Flush: All cards of the same suit.
;; Full House: Three of a kind and a pair.
;; Four of a Kind: Four cards of the same value.
;; Straight Flush: All cards are consecutive values of same suit.
;; Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
;; The cards are valued in the order:
;; 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

;; If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights
;; beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then
;; highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

;; The file, poker.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards
;; (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume  that all
;; hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

;;How many hands does Player 1 win?


;(use 'sage)
(use 'clojure.repl)
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set]) 

(def expt clojure.math/expt)
(def join clojure.string/join)
(def trim clojure.string/trim)
(def split clojure.string/split)
(def split-lines clojure.string/split-lines)

(def & comp)
(def p partial)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn make-hand [basic-hand]
  (let [hand (sort-by first > basic-hand)
        order  (map first hand)
        freqs  (sort-by first > (map reverse (frequencies (map first hand))))
        flush? (apply = (map second hand))
        straight? (and (= 4 (- (first order) (last order)))
                       (= 5 (count (distinct order))))]
    {:hand hand, :order order, :freqs freqs, :flush? flush? :straight? straight?}))

(defn royal-flush? [hand]
  (if (and (hand :flush?) (hand :straight?) (= 14 (first (hand :order))))
    [10 '()]
    nil))

(defn straight-flush? [hand]
  (if (and (hand :flush?) (hand :straight?))
    [9 (hand :order)]
    nil))

(defn four-kind? [hand]
  (if (= 4 (first (first (hand :freqs))))
    [8 (map second (hand :freqs))]
    nil))

(defn full-house? [hand]
  (if (and (= 3 (first (first (hand :freqs))))
           (= 2 (first (second (hand :freqs)))))
    [7 (map second (hand :freqs))]
    nil))

(defn flush? [hand]
  (if (hand :flush?)
    [6 (hand :order)]
    nil))

(defn straight? [hand]
  (if (hand :straight?)
    [5 (hand :order)]
    nil))

(defn three-kind? [hand]
  (if (= 3 (first (first (hand :freqs))))
    [4 (map second (hand :freqs))]
    nil))

(defn two-pairs? [hand]
  (if (and (= 2 (first (first (hand :freqs))))
           (= 2 (first (second (hand :freqs)))))
    [3 (map second (hand :freqs))]
    nil))

(defn pair? [hand]
  (if (= 2 (first (first (hand :freqs))))
    [2 (map second (hand :freqs))]
    nil))

(defn high-card [hand]
  [1 (hand :order)])

(defn score-hand [hand]
  (first
   (drop-while empty? (map #(% hand)
                           [royal-flush?, straight-flush?, four-kind?, full-house?, flush?, straight?, three-kind?, two-pairs?, pair?, high-card]))))

(defn winner [hand-1 hand-2]
  (let [s-1 (first hand-1), s-2 (first hand-2)
        v-1 (second hand-1), v-2 (second hand-2)]
    (cond (> s-1 s-2) :win
          (< s-1 s-2) :lose
           :else
           (let [vs (first (drop-while zero? (map compare v-1 v-2)))]
             (cond (= 1 vs) :win
                   (= -1 vs) :lose
                   :else :tie)))))

(defn euler-54 []
  (let [file       (seq (join " " (split-lines (slurp "src/poker.txt"))))
        no-face    (join (replace {\T 10, \J 11, \Q 12, \K 13, \A 14} file))
        cards      (map (fn [[n s]] [(Integer. n) s])  (map rest (re-seq #"(\d\d?)(\w)\W?" no-face)))
        hands      (map (comp score-hand make-hand vec) (partition 5 cards))
        games      (map vec (partition 2 hands))]
    (count
     (filter #{:win}
             (map (fn [hands] (winner (first hands) (second hands)))
                  games)))))

