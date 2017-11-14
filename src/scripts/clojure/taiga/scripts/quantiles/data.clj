(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :since "2017-11-10"
      :date "2017-11-13"
      :doc "Generate data for mean regression training,
            probability measure regression training,
            and test."}
    
    taiga.scripts.quantiles.data
  
  (:require [clojure.java.io :as io]
            [zana.api :as z]
            [taiga.scripts.quantiles.record :as record]
            [taiga.scripts.quantiles.defs :as defs]))
;;----------------------------------------------------------------
(let [median (record/make-pyramid-function 16.0)
      data (z/seconds 
             "generate"
             (z/map (record/generator median) 
                    (range (* 3 (long defs/n))))) 
      [mean emp test] (z/seconds 
                        "partition" 
                        (z/partition defs/n data))]
  (z/seconds
    "mean" 
    (z/write-tsv-file
      record/tsv-attributes 
      mean 
      (defs/input-file "mean" (z/count mean) "tsv.gz")))
  (z/seconds
    "emp" 
    (z/write-tsv-file
      record/tsv-attributes 
      emp 
      (defs/input-file "emp" (z/count emp) "tsv.gz")))
  (z/seconds
    "test" 
    (z/write-tsv-file
      record/tsv-attributes 
      test 
      (defs/input-file "test" (z/count test) "tsv.gz"))))
;;----------------------------------------------------------------