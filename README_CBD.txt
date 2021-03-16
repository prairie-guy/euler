3/2/21

Clojure Configuration Files:
================================
NOTE: Configuation files (deps.edn, README_CBD.txt, src/cbd.clj, TEMPLATE.clj) all live in ~/.clojure/ linked to ~/scratch/euler/
Regular github backup should make copies of main files as backup.

~/.clojure/deps.edn
==================
clj now works from the command line. It uses a standard 'edn' type configuration files.
These can be located in the directory where the code is located or here '.clojure/deps.edn'
The file in this directory is the global file, unless otherwise shadowed by a local version.

deps.edn: ~/.clojure/deps.edn
{
 :paths ["/home/cdaniels/.clojure/src"]

 :deps {
        org.clojure/math.numeric-tower {:mvn/version "0.0.4"}
        org.clojure/math.combinatorics {:mvn/version "0.1.6"}
        org.clojure/java.classpath {:mvn/version "1.0.0"}
        org.clojure/data.json {:mvn/version "1.0.0"}
        org.clojure/core.match {:mvn/version "1.0.0"}
        org.clojure/data.csv {:mvn/version "1.0.0"}
        tupelo/tupelo {:mvn/version "21.01.26b"}
        }
}

Comments:
=============
tupelo/tupelo has extremely useful functions and macros to expand the clojure capabilities.
Its tupelo.core functions are generally imported without module reference.

TEMPLATE.clj
=============
Default require and use files for a new file.

~/.clojure/src
=============

This sub directory contains libray and configuration files used by clojure, notably, cbd.clj


================================
/home/cdaniels/.clojure/src/cbd.clj
;; CBD Module for Various Personal Utilities
;; 3/2/2021


Other Sources of Files
================================
Discrete-math functions: git@github.com:tlberglund/discrete-math.git
Missing pure funtions: https://github.com/weavejester/medley
More useful core funtions: https://github.com/marick/suchwow
Declarative style: https://github.com/plumatic/plumbing
