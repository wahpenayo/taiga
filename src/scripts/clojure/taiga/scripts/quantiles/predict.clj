(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :since "2017-11-10"
      :date "2017-11-15"
      :doc "Predict quantiles and compute quantile code,,
            given a probability measure regression forest."}
    
    taiga.scripts.quantiles.predict
  
  
  (:require [zana.api :as z]
            [taiga.scripts.quantiles.defs :as defs]))
;;----------------------------------------------------------------
(doseq [prefix ["mean" "measure" "test"]]
  (z/seconds 
    (print-str *ns* prefix)
    (defs/predict defs/n prefix)))
;;----------------------------------------------------------------