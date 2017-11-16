(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :since "2017-11-10"
      :date "2017-11-15"
      :doc "Train a mean regression forest."}
    
    taiga.scripts.quantiles.mean
  
  (:require [zana.api :as z]
            [taiga.scripts.quantiles.defs :as defs]))
;;----------------------------------------------------------------
(z/seconds 
  (str *ns*)
  (defs/mean-regression defs/n))
;;----------------------------------------------------------------