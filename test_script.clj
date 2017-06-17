;; Embedded Scripting/ Scripting/ Matching

(use '[clojure.java.shell :only [sh]])

(use '[clojure.core.match :only (match)])


(defn three_five_long [upper]
  (for [n (range 1 upper)]
    (match [(mod n 3) (mod n 5)]
           [0 0] [n, "3/5"]
           [0 _] [n, "3"  ]
           [_ 0] [n, "5"]
           :else n)))

(defn three_five [upper]
  (remove nil?
   (for [n (range 1 upper)]
     (match [(mod n 3) (mod n 5)]
            [0 0]  n
            :else nil))))
