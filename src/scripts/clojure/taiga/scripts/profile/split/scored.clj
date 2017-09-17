(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-11-23"}
    
    taiga.scripts.profile.split.scored
  
  (:require [taiga.split.numerical.categorical.scored :as scored]
            [taiga.scripts.profile.split.defs :as defs]))
;; clj src\scripts\clojure\taiga\scripts\profile\split\scored.clj
;;------------------------------------------------------------------------------
(println "scored")
(defs/bench scored/split (defs/primate-options))
(defs/bench scored/split (defs/kolor-options))
;;------------------------------------------------------------------------------
