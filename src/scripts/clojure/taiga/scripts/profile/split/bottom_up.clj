(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-11-23"}
    
    taiga.scripts.profile.split.bottom-up
  
  (:require [taiga.split.object.categorical.bottom-up :as bottom-up]
            [taiga.scripts.profile.split.defs :as defs]))
;; clj src\scripts\clojure\taiga\scripts\profile\split\bottom_up.clj
;;------------------------------------------------------------------------------
(println "bottom up")
(defs/bench bottom-up/split (defs/primate-options))
(defs/bench bottom-up/split (defs/kolor-options))
;;------------------------------------------------------------------------------
