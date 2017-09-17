(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-11-23"}
    
    taiga.scripts.profile.split.cutoff
  
  (:require [taiga.split.object.categorical.all-subsets :as all-subsets]
            [taiga.split.object.categorical.bottom-up :as bottom-up]
            [taiga.scripts.profile.split.defs :as defs]))
;; clj src\scripts\clojure\taiga\scripts\profile\split\cutoff.clj > splits.txt
;;------------------------------------------------------------------------------
(let [primates (defs/primate-options)]
  (println "bottom up")
  (defs/bench bottom-up/split primates)
  (println "all-subsets")
  (defs/bench all-subsets/split primates))
;;------------------------------------------------------------------------------
