(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-12-19"}
    
    taiga.scripts.profile.split.all-subsets
  
  (:require [taiga.split.object.categorical.all-subsets :as all-subsets]
            [taiga.scripts.profile.split.defs :as defs]))
;; clj src\scripts\clojure\taiga\scripts\profile\split\all_subsets.clj
;;------------------------------------------------------------------------------
(println "all-subsets")
(defs/bench all-subsets/split (defs/primate-options))
(defs/bench all-subsets/split (defs/kolor-options))
;;------------------------------------------------------------------------------
