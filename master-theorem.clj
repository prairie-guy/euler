;; Master Therorm

(use 'clojure.string)

(defn pos [char] (- (int char) 96))
(defn ch [p] (char (+ 96 p)))

(defn shift [string n] (join
                     (map #(char (+ 97 %))
                          (map #(mod % 26)
                               (map #(+ n %)
                                    (map #(- (int %) 97) string))))))

(defn word-to-pos [string] (map pos string))

;;; Move-to-Beat
(defn move-to-beat []
  (let [score "[a (+ f g) (+ (* e 3) (* b 3)) (+ c e g d) (* c 3) (/ f 2) (/ b 2) (* c 4) (* e 3) f (+ a e) e (+ (* d 2) (* e 2)) (+ g b) (* g 2) g]"
        alpha (into #{} (for [i (range (int \a) (+ 26 (int \a)))] (char i)))]
    (join
     (map ch
          (eval (read-string 
                 (join (map #(if (alpha %) (pos %) %) score))))))))

;; Ancient Crossword Puzzle
(def sator [[\s \a \t \o \r]
            [\a \r \e \p \o]
            [\t \e \n \e \t]
            [\o \p \e \r \a]
            [\r \o \t \a \s]])
(def sq [
         [\l \a \d \d \g]
         [\a \e \e \m \o]
         [\i \k \o \k \i]
         [\o \m \e \e \a]
         [\g \d \d \a \l]
         ])

(def sq-half "laddgaeemoiko")


(def sq-1 [
           [\o \m \e \g \a]
           [\i \d \d \a \l]
           [\e \k \o \k \e]
           [\l \a \d \d \i]
           [\a \g \e \m \o]           
           ])

(def sq-2 [
           [\l \a \d \e \g]
           [\a \d \m \o \e]
           [\i \k \o \k \i]
           [\e \o \m \d \a]
           [\g \e \d \a \l]
           ])
  
(defn print-sq [square] (doseq [row square] (prn (seq row))))
(defn freq [square] (frequencies (join (map join square))))

