;;  julia.clj
;;  v1.0
;;
;;  C. Bryan Daniels
;;  February, 2014
;;
;;  The julia.clj library is a very simple wrapper around julia and allows julia commands to be executed
;;  within clojure, with the results returned as strings. Converting these strings to clojure types will
;;  generally note work. (The :eval command attempts to do a a best-efforts-basis. See below)
;;

(ns julia
  (:require [clojure.string :refer (join trim split split-lines)])
  (:require [clojure.java.shell :refer (sh)]))

(defn cj [code-str]
  (trim
   (:out
    (sh "/usr/local/bin/julia" "-E" code-str))))
