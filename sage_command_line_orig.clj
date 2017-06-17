#!/usr/bin/env cake
;;
;; Several very basic functions to call sage functions from clojure
;;
;; (sage "factorial(100)")
;;  
;; (sage "x = var('x'); diff(sin(x^2), x, 4)")
;;
;; sage expressions are seperated by ';'
;;
;; A print command is automatically appended to the last expression of a sage command
;;
;; Primary funcitons are: sage and sage-str


(use 'clojure.contrib.math)
(require 'clojure.string)

(defn- cleanup-sage-output [s]
  "Clean up header and removes '\n'"
  (clojure.string/replace 
   (clojure.string/replace
    s #"\n" "")
   #"^Detected.*mode" ""))

(defn- add-print [s]
  "Adds a print statement to the final expression of
command-string passed to sage"
  (cond
   (re-seq #"print" s) s
   (not (re-seq #";" s)) (clojure.string/replace s #"^" "print ")
   :else (let [last-exp (last  (re-matches #"(^.*;)(.*$)" s))]
           (clojure.string/replace s last-exp (str " print " last-exp)))))

(defn sage-str [command-str]
  "Returns string output from passing a command-str to sage"
  (cleanup-sage-output
   (:out (clojure.java.shell/sh "sage" "-c"  (add-print command-str)))))

(defn sage-eval [command-str]
  "Read string ouput from passing a command-str to sage.
If it fails, it throws an exception"
  (read-string (sage-str command-str)))

(defn sage [command-str]
  "Tries to read string expression.
If it fails, it returns a string expression"
  (try (sage-eval command-str)
       (catch Exception e (sage-str command-str))))
