#!/usr/bin/env cake
;;
;; euler17.clj
;;
;; If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
;;
;; If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
;;
;; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters
;; and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
;;
;; Answer = 21124

(require '[cbd])
(use 'clojure.contrib.math)


(defn spell-num [num]
  (let [nw-map {1 "one", 2 "two", 3 "three", 4 "four", 5 "five", 6 "six", 7 "seven", 8 "eight", 9 "nine", 10 "ten",
                11 "eleven", 12 "twelve",  13 "thirteen", 14 "fourteen", 15 "fifteen", 16 "sixteen", 17 "seventeen", 18 "eighteen", 19  "nineteen",
                20 "twenty", 30 "thirty", 40 "forty", 50 "fifty", 60 "sixty", 70 "seventy", 80 "eighty", 90 "ninety"}
        hs (quot num 100), ts (rem num 100)]
    
    (if (= num 1000)
      "one-thousand"
      (str
       (cond (and (> hs 0) (= ts 0)) (str (nw-map hs) "-hundred")
             (> hs 0) (str (nw-map hs) "-hundred-and-")
             :else "")
       (cond (= ts 0) ""
             (contains? nw-map ts)  (nw-map ts)
             :else (str (nw-map (* 10 (quot ts 10))) "-"  (nw-map (rem ts 10))))))))

(defn count-letters [spelled-out-word]
  (count (remove #(= \- %) spelled-out-word)))

(prn (reduce + (map #((comp count-letters spell-num) %) (range 1 1001))))

           
        

