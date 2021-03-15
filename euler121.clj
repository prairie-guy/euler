(use 'tupelo.core)
(use '[cbd])
(require '[clojure.math.numeric-tower :as cmath])
(require '[clojure.math.combinatorics :as comb])

;; Euler 121 Disc game prize fund
;; #
;; A bag contains one red disc and one blue disc.
;; In a game of chance a player takes a disc at random and its colour is noted.
;; After each turn the disc is returned to the bag, an extra red disc is added,
;; and another disc is taken at random.
;; #
;; The player pays 1 to play and wins if they have taken
;; more blue discs than red discs at the end of the game.
;; #
;; If the game is played for four turns, the probability of a player winning
;; is exactly 11/120, and so the maximum prize fund the banker should allocate
;; for winning in this game would be £10 before they would expect to incur a loss.
;; Note that any payout will be a whole number of pounds and also includes the
;; original £1 paid to play the game, so in the example given the player actually wins £9.
;; #
;; Find the maximum prize fund that should be allocated to a single game
;; in which fifteen turns are played.
;;           1       2       3       4
;;           =========================
;;   R       1       2       3       4
;;   B       1       1       1       1
;;   T       2       3       4       5
;;   P(B)    1/2     1/3     1/4     1/5
;;           *       *       *       4/5     4/120
;;   P(B>R)  *       *       3/4     *       3/120
;;           *       2/3     *       *       2/120
;;           1/2     *       *       *       1/120
;;           *       *       *       *       1/120
;;
;;          C(3,1) -> C(3,0)+C(3,1)+C(3,2)+C(3,3) = 1 + 3 + 3 + 1 = 8
;;           Rolls
;;           =======================
;;           4       (****)+(***-)
;;           5       (*****)+(****-)+(***--)
;;           6       (******)+(*****-)+(****--)
;;           7       (*******)+(******-)+(*****--)+(****---)
;;           8       (********)+(*******-)+(******--)+(*****---)
;;
;;   C(5,5)=1      (1/2*1/3*1/4*1/5)+  -> 1/120
;;                 (1)

;;   C(5,4)=5      (1/2*1/3*1/4*1/5*1/6)+(1/2*2/3*1/4*1/5*1/6)+(1/2*1/3*3/4*1/5*1/6)+(1/2*1/3*1/4*4/5*1/6)+(1/2*1/3*1/4*4/5*5/6)= (1+2+3+4+5)/120
;;                                                                                                                              = 15/120 = 1/8
;;                 (1)                   (2)                   (3)                   (4)                   (5)

;;   C(5,3)=10     (1/2*2/3*1/4*1/5*1/6) (1/2*1/3*3/4*1/5*1/6) (1/2*1/3*1/4*4/5*1/6) (1/2*1/3*1/4*1/5*5/6) (1/2*2/3*3/4*1/5*1/6)
;;                 (1/2*2/3*1/4*4/5*1/6) (1/2*2/3*1/4*1/5*5/6) (1/2*1/3*3/4*4/5*1/6) (1/2*1/3*3/4*1/5*5/6) (1/2*1/3*1/4*4/5*5/6)
;;                 (--***)               (-*-**)               (-**-*)               (-***-)               (*--**)
;;                 (*-*-*)               (*-**-)               (**--*)               (**-*-)               (***-)
;;
;;                 (1*2)                 (1*3)                 (1*4)                 (1*5)                  (2*3)
;;                 (2*4)                 (2*5)                 (3*4)                 (3*5)                  (4*5)               = 80/120 = 17/24
;;                  1/120 + 1/8 + 17/24

;; prob(Choosing k blues in n turns)
(defn blue-prob [n k]
  (let [all-prob (product 2N (inc n))]
    (if (= k n) (/ 1 all-prob)
        (it-> (comb/combinations (range 1 (inc n)) (- n k))
              (map prod it)
              (sum it)
              (/ it all-prob)))))
;; prob(winning a game with blue>red) and max-payout by Bank
(defn game [n]
  (it-> (for [i (range (cmath/ceil (/ n 2.0)))] ;
          (blue-prob n (- n i)))
        (sum it)
        (spy it :blue-prob)
        (/ (- (denominator it) (numerator it))
           (numerator it))
        (cmath/ceil it)
        (spy it :max-payout)))

;; user> (time(game 15))
;; :Prob_blue => 9219406943/20922789888000
;; :Payout => 2269N
;; "Elapsed time: 33.305623 msecs"
;;  => 2269N
