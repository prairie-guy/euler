; #!/usr/bin/env cake
;;
;; euler15.clj
;;
;;

(use '[cbd])
(use 'clojure.contrib.math)


(defn paths [n]
  ; "Returns # of monotonic paths through grid of n x n size"
  (comb  (* 2 n) n,))

(prn (paths 20))

(this is a tesat)