;;  sage.clj
;;  v1.0
;;
;;  C. Bryan Daniels
;;  June 25, 2011
;;
;;  Sage is a free open-source mathematics software system licensed under the GPL.
;;  It combines the power of many existing open-source packages into a common Python-based interface.
;;
;;  The sage.clj library is a very simple wrapper around sage and allows sage commands to be executed
;;  within clojure, with the results returned as strings. Converting these strings to clojure types will
;;  generally note work. (The :eval command attempts to do a a best-efforts-basis. See below)
;;
;;  Download and install 'sage' from http://www.sagemath.org/
;;  Configure sage so that at the command-line it can be started with: 'sage'
;;  sage.clj depends upon a http-server provided by sage known as 'sage_notebook'
;;  There is no need to set up an account. A sage test_notebook server is utilized.
;;  By default sage's test_notebook runs under the account 'admin' with an arbitrary password.
;;
;;  To avoid overflowing the sage_notebook server with large calculations consider editing the following file:
;;
;;  ~/sage/devel/sagenb/sagenb/notebook/cell.py
;;  Change: 
;; 'MAX_OUTPUT = '32000'    to 'MAX_OUTPUT = 320000000'
;; 'MAX_OUTPUT_LINES = 120' to 'MAX_OUTPUT_LINES = 12000'
;;
;;  The commands 'sage-start-server' and 'sage-stop-server' are platform dependent. They have been tested on
;;  Mac OS X and should work on Linux. For Windows, these commands needs to be implemented and tested. (It should be easy.)
;;
;;  Example Usage:
;;  (def s (sage))                           -> #'user/s
;;  (s "2+2")                                -> ["4"]
;;  (s :value "2+2")                         -> "4"
;;  (s :eval  "2+2")                         -> 4
;;  (s "2+2; factorial(10)")                 -> "3628800"
;;  (s :eval "2+2")                          ->  4
;;  (s "2+2; factorial(10)")                 -> ["4" "3628800"]
;;  (s :value "2+2; factorial(10)")          -> "3628800"
;;  (s :eval "2+2; factorial(10)")           -> 3628800
;;  (s :eval "list(factor(factorial(10)))")  -> [(2 8) (3 4) (5 2) (7 1)]
;;  (s :eval "prime_range(10)")              -> [2 3 5 7]
;;  (s :raw "2+2")                           -> "{\n\"status\": \"done\",\n\"files\": [],\n\"cell_id\": 6\n}\n___S_A_G_E___\n\n4\n"
;;  (s :logout                               -> "logged out"
;;  (s :stop-server)                         -> "server stopped"
;;

(ns sage
  (:require [clojure.string :only (replace replace-first split)])
  (:require [clj-http.client :as client])
  (:require [clojure.java.shell])
  (:import (java.net URLEncoder)))

(def admin-passwd "chicago")
(def default-port 8000)
(def ins-delimiter "~~~~")
(def del-delimiter (re-pattern ins-delimiter))

(defn- sage-server-up? [port]
  (try (if (client/get (str "http://localhost:" port)) true)
       (catch Exception e false)))

(defn- sage-start-server [port]
  "System dependent. Only works with unix/osx -- need a  Window's version."
      (if (not (sage-server-up? port))
       (let [thrd (Thread. #(clojure.java.shell/sh "sage" "-c" (str "from sagenb.notebook.notebook_object import test_notebook; test_notebook('" admin-passwd "', port=" port ")")))]
         (.start thrd)
         (while (not (sage-server-up? port)) (Thread/sleep 500))
         (str "sage started on port " port))
     (str "sage previously started")))

(defn sage-stop-server [port]
  "System dependent. Only works with unix/osx -- need a Window's version. BUG: Any call to 'sage-stop-server' stops servers on all ports"
  (if (sage-server-up? port)
    (let [raw (:out (clojure.java.shell/sh "ps" "aux"))
          sages (re-seq #".*/sage.*" raw)
          ps (map first (for [s sages] (re-seq #" [0-9]*" s)))]
      (dorun (map #(clojure.java.shell/sh "kill" "-9" %) ps))
      (str "sage stopped on port " port))
    (str "sage previously stopped")))

(defn- sage-login
  ([port] (sage-login "admin" admin-passwd port))
  ([user-name password port]
     (let [raw (:body (client/get (str "http://localhost:" port "/simple/login?username=" user-name "&password=" password)))
           session (last (first  (re-seq #": \"(.*)\"" raw)))]
       {:session session :port port :raw raw})))

(defn- sage-logout [login-map]
  (let [{:keys [session port]} login-map,
        raw (:body (client/get (str "http://localhost:" port
                                     "/simple/logout?session=" session)))]
    raw))

(defn- sage-http-raw [login-map sage-command]
  (let [{:keys [session port]} login-map
        split-expressions (fn [s] (clojure.string/replace s  ";" (str ";print '" ins-delimiter"';")))]
       (:body (client/get (str "http://localhost:" port
                               "/simple/compute?session=" session
                               "&code=" (URLEncoder/encode (split-expressions sage-command))
                               "&timeout=600")))))

(defn- sage-http [login-map sage-command]
     (let [{:keys [session port]} login-map
           trim-header       (fn [s] (clojure.string/replace-first s #"(?s).*___S_A_G_E___\n\n" ""))
           eliminate-newline (fn [s] (clojure.string/replace s #"\n" ""))
           parse-results     (fn [s] (clojure.string/split (eliminate-newline (trim-header s))  del-delimiter))]
       (parse-results (sage-http-raw login-map sage-command))))

(defn- fix-single-quote [s] (clojure.string/replace s #"'" "\""))
(defn- fix-map [s] (clojure.string/replace s #":" ""))
;;(defn- fix-set [s] (let [raw (re-seq #"set\((.*)\)" s)] (if raw 

(defn- sage-eval [sage-value]
  (-> sage-value
      (fix-single-quote)
      (fix-map)
      read-string))

(defn make-sage
  ([] (make-sage default-port))
  ([port]
  (let [login-map (ref {})]
    (fn [cmd & sage-expres]
      (condp = cmd
          :server-up? (sage-server-up? port)
          :start-server (sage-start-server port)
          :stop-server (sage-stop-server port)
          :login (let [s (sage-login port)]
                   (dosync (ref-set login-map s))
                   (str "logged in"))
          :logout (do (sage-logout @login-map)
                      (str "logged out"))
          :login-map @login-map
          :raw    (apply sage-http-raw (concat [@login-map] sage-expres))
          :values (apply sage-http (concat [@login-map] sage-expres))
          :value  (last (apply sage-http (concat [@login-map] sage-expres)))
          :eval   (sage-eval (last (apply sage-http (concat [@login-map] sage-expres))))
          (last (sage-http @login-map cmd)))))))

(defn sage
  ([] (sage default-port))
  ([port]
  (let [s (make-sage port)]
    (s :start-server)
    (s :login)
    s)))

(defn with-sage [sage-expres]
  (let [s (sage)
        result (s sage-expres)]
    (s :logout)
;;    (s :stop-server)
    result))

