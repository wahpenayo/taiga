(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :since "2017-11-10"
      :date "2017-11-15"
      :doc "Generate data for mean regression training,
            probability measure regression training,
            and test."}
    
    taiga.scripts.quantiles.data
  
  (:require [zana.api :as z]
            [taiga.scripts.quantiles.defs :as defs]))
;;----------------------------------------------------------------
(z/seconds 
  (str *ns*)
  (defs/generate-data defs/n))
;;----------------------------------------------------------------
