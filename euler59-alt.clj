;;
;; euler59.clj
;;

(use '[clojure.string :only (join trim split split-lines)])
(use '[clojure.math.combinatorics :only (permutations)])

(def euler59-cipher (map #(Integer. %) (split (trim (slurp "src/cipher1.txt")) #",")))

(def key-space (for [i (range (int \a) (inc (int \z))) ] [i] ))

(def re-char-seqs #" ")

(defn decode [key cipher]
  (join
   (map (comp char bit-xor) cipher (take (count cipher) (cycle key)))))

(defn score [decoded char-seqs]
  (count (re-seq char-seqs decoded)))

(defn decipher
  ([cipher key-space char-seqs] (decipher cipher key-space char-seqs 1))
  ([cipher key-space char-seqs n]
     (take-last n
      (sort
       (for [key key-space :let [dc (decode key cipher), s (score dc char-seqs)]]
         [s key dc])))))

(defn euler59 []
  (let [keys (flatten (map second (decipher euler59-cipher key-space  re-char-seqs 3))),
        key-space-3 (permutations keys)
        answer (last (last (decipher euler59-cipher key-space-3 re-char-seqs)))]
    [(reduce + (map int answer)) answer]))

