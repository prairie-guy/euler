#!/usr/bin/env cake
;;
;; euler22.clj
;;
;; Using euler22.txt, a 46K text file containing over five-thousand first names,
;; begin by sorting it into alphabetical order. Then working out the alphabetical
;; value for each name, multiply this value by its alphabetical position in the
;; list to obtain a name score.
;;
;; For example, when the list is sorted into alphabetical order,
;; COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
;; So, COLIN would obtain a score of 938 *  53 = 49714.
;;
;; What is the total of all the name scores in the file?
;;
;; Answer = 871198282
;;


(use '[cbd])
(use 'clojure.contrib.math)
(require 'clojure.string)


(def raw-text
  (slurp "src/euler22.txt"))

(def parsed-text
  (sort 
   (clojure.string/split 
    (clojure.string/replace raw-text #"\"" "")
    #",")))

(defn score [word]
  (reduce + (map #(- (int %) 64) (seq word))))

(defn run []
  (reduce +
          (map-indexed (fn [x y] (* (inc x) (score y)))
                       parsed-text)))
