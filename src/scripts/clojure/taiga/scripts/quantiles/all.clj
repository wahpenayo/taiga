(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :since "2017-11-10"
      :date "2017-11-15"
      :doc "Complete data generation, train, test."}
    
    taiga.scripts.quantiles.all
  
  (:require [zana.api :as z]
            [taiga.scripts.quantiles.defs :as defs]))
;;----------------------------------------------------------------
(println (str *ns*) defs/n defs/nterms)
(z/seconds 
  "data"
  (defs/generate-data defs/n))
(z/seconds 
  "mean"
  (defs/mean-regression defs/n))
(z/seconds 
  "measure"
  (defs/real-probability-measure defs/n))
(doseq [prefix ["mean" "measure" "test"]]
  (z/seconds 
    (print-str "predict" prefix)
    (defs/predict defs/n prefix)))
(doseq [prefix ["mean" "measure" "test"]]
  (z/seconds 
    (print-str "cost" prefix)
    (defs/relative-cost defs/n prefix)))
;;----------------------------------------------------------------
