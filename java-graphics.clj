(require '[clojure.repl :as repl] )

(require '[clojure.java.javadoc :refer (javadoc) ] )

(defn xors
  [max-x max-y]
  (for [x (range max-x), y (range max-y)]
    [x y (bit-xor x y)] ))




;; (filter #(re-find #"Vis" %) (for [meth (.getMethods java.awt.Frame)
;;                        :let [name (.getName meth)]]
;;                    name))


(def frame (java.awt.Frame.))

(.setVisible frame true)
(.setSize frame (java.awt.Dimension. 200 200))
(def gfx (.getGraphics frame))



(.setColor gfx (java.awt.Color. 255 128 0))

(.fillRect gfx 100 100 75 50)

(doseq [[x y xor] (xors 200 200)]
  (.setColor gfx (java.awt.Color. xor xor xor))
  (.fillRect gfx x y 1 1))
