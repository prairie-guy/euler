(defn mix [& lsts]
  (defn mix-helper [lsts result]
    (if (empty? lsts)
      result
      (mix-helper (rest lsts) (for [i (first lsts) j result] (cons i j)))))
  (let [lsts (reverse lsts)]
    (mix-helper (rest lsts) (for [i (first lsts)] (list i)))))

;; No duplication
(defn permute [& lsts]
  (defn permute-helper [lsts result]
    (if (empty? lsts) result
        (permute-helper (rest lsts) (for [i (first lsts), j result :when (not (some #(= i %)  j))] (cons i j)))))
  (let [lsts (reverse lsts)]
    (permute-helper (rest lsts) (for [i (first lsts)] (list i)))))

(defn permutations [choose lst]
  (if (= 0 choose)
    lst
    (apply permute (for [i (range choose)] lst))))

(defn combine [& lsts]
  (defn combine-helper [lsts result]
    (if (empty? lsts) (distinct (map #(sort < %) result))
        (combine-helper (rest lsts) (for [i (first lsts), j result :when (not (some #(= i %)  j))] (cons i j)))))
  (combine-helper (rest lsts) (for [i (first lsts)] (list i))))

(defn combinations [choose lst]
  (if (= 0 choose)
    lst
    (apply combine (for [i (range choose)] lst))))
                                                       
(defn subsets [lst]
  (if (empty? lst)
    (list '())
    (let [r (subsets (rest lst))]
      (concat r (map (fn [x] (cons (first lst) x)) r)))))
