(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-12-19"}
    
    taiga.scripts.profile.split.all
  
  (:require [taiga.split.numerical.categorical.scored :as scored]
            [taiga.split.object.categorical.all-subsets :as all-subsets]
            [taiga.split.object.categorical.bottom-up :as bottom-up]
            [taiga.split.object.categorical.heuristic :as heuristic]
            [taiga.scripts.profile.split.defs :as defs]))
;; clj src\scripts\clojure\taiga\scripts\profile\split\all.clj > splits.txt
;;------------------------------------------------------------------------------
(let [primates (defs/primate-options)
      kolors (defs/kolor-options)]
(println "scored")
(defs/bench scored/split primates)
(defs/bench scored/split kolors)
(println "all-subsets")
(defs/bench all-subsets/split primates)
(defs/bench all-subsets/split (defs/kolor-options))
(println "bottom up")
(defs/bench bottom-up/split primates)
(defs/bench bottom-up/split kolors)
;;------------------------------------------------------------------------------
