#!/usr/bin/env cake
;;
;; euler59.clj
;;
;;

;; Each character on a computer is assigned a unique code and the preferred standard is ASCII
;; (American Standard Code for Information Interchange). For example, uppercase A = 65,
;; asterisk (*) = 42, and lowercase k = 107.

;; A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each
;; byte with a given value, taken from a secret key. The advantage with the XOR function is that
;; using the same encryption key on the cipher text, restores the plain text; for example,
;; 65 XOR 42 = 107, then 107 XOR 42 = 65.

;; For unbreakable encryption, the key is the same length as the plain text message, and the key
;; is made up of random bytes. The user would keep the encrypted message and the encryption key
;; in different locations, and without both "halves", it is impossible to decrypt the message.

;; Unfortunately, this method is impractical for most users, so the modified method is to use
;; a password as a key. If the password is shorter than the message, which is likely, the key
;; is repeated cyclically throughout the message. The balance for this method is using a
;; sufficiently long password key for security, but short enough to be memorable.

;; Your task has been made easy, as the encryption key consists of three lower case characters.
;; Using cipher1.txt (right click and 'Save Link/Target As...'), a file containing the encrypted
;; ASCII codes, and the knowledge that the plain text must contain common English words, decrypt
;;the message and find the sum of the ASCII values in the original text.

;(use 'sage)
(use 'clojure.repl)
(use '[clojure.string :only (join trim split split-lines)])
(require 'cbd)
(require '[clojure.math.numeric-tower :as clojure.math])
(require '[clojure.math.combinatorics :as clojure.combinatorics])
(require '[clojure.set])

(defn decode [key cipher]
  "Takes a key (a vector of ascii codes) and a cipher (composed of ascii code an sequence),
   then cyclically 'xor's the key with the cipher sequence and returns a string of ascii characters."
  (clojure.string/join
   (map (comp char bit-xor) cipher (take (count cipher) (cycle key)))))

(defn score [decoded char-seqs]
  "The score of a decoded string is how many char-seqs it contains.
   char-seqs is a re-exp comprised of an alteration of possible character sequences"
  (count (re-seq char-seqs decoded)))

(defn decipher
  "Deciphers returns a seq of the n-highest scoring decoded ciphers"
  ([key-space cipher char-seqs] (decipher key-space cipher char-seqs 1))
  ([key-space cipher char-seqs n]
     (take-last n
      (sort
       (for [key key-space :let [dc (decode key cipher), s (score dc char-seqs)]
             :when (> s (* 0.02 (count cipher)) )]
         [s key dc])))))

(def euler59-cipher
  "Converts the cipher1.txt file of ascii codes into a sequence."
  (map #(Integer. %)
       (split (trim (slurp "src/cipher1.txt")) #",")))

(def key-space-3
  "Returns vectors length-3 of all possible keys of 3 ascii codes for the lower-case letters."
  (for [i (range (int \a) (inc (int \z))),
        j (range (int \a) (inc (int \z))),
        k (range (int \a) (inc (int \z)))]
    [i j k]))

(defn euler59 []
  (let [deciphered (last (last (decipher key-space-3 euler59-cipher #"the|and|to|of|in|is")))]
    [(reduce + (map int deciphered))
     deciphered]))

(def key-space-1
  "Returns a length-1 vector comprised of all the lower case letters"
  (for [i (range (int \a) (inc (int \z))) ] [i] ))

(defn euler59-alt []
  "Much faster than euler59 because key-space-1 contains 26 keys vs. 17,576 in key-space-3"
  (let [key (flatten (map second (decipher key-space-1 euler59-cipher #" " 3))),
        keys (clojure.combinatorics/permutations key)]
    (last (last (decipher keys euler59-cipher #" ")))))

;; [107359 "(The Gospel of John, chapter 1) 1 In the beginning the Word already existed. He was with God, and he was God. 2 He was in the beginning with God. 3 He created everything there is. Nothing exists that he didn't make. 4 Life itself was in him, and this life gives light to everyone. 5 The light shines through the darkness, and the darkness can never extinguish it. 6 God sent John the Baptist 7 to tell everyone about the light so that everyone might believe because of his testimony. 8 John himself was not the light; he was only a witness to the light. 9 The one who is the true light, who gives light to everyone, was going to come into the world. 10 But although the world was made through him, the world didn't recognize him when he came. 11 Even in his own land and among his own people, he was not accepted. 12 But to all who believed him and accepted him, he gave the right to become children of God. 13 They are reborn! This is not a physical birth resulting from human passion or plan, this rebirth comes from God.14 So the Word became human and lived here on earth among us. He was full of unfailing love and faithfulness. And we have seen his glory, the glory of the only Son of the Father."]
