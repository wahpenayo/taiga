(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-05"
      :doc "data from R quantreg for regression tests." }
    
    taiga.test.regress.stackloss.stackloss
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]))
;;----------------------------------------------------------------
(z/define-datum StackLoss
  [^double airflow
   ^double watertemp
   ^double acidconc
   ^double stackloss
   ^double predicted-stackloss])
;;----------------------------------------------------------------
(defn options [] 
  {:data (read-tsv-file (io/file "data" "stackloss.csv") #",")
   :attributes {:airflow airflow
                :watertemp watertemp
                :acidconc acidconc
                :ground-truth stackloss
                :prediction predicted-stackloss}
   :embedding (z/affine-embedding
                "stackloss"
                [[:airflow Double/TYPE]
                 [:watertemp Double/TYPE]
                 [:acidconc Double/TYPE]])
   :nterms 1023
   :mincount 1
   :mtry (int (Math/round (Math/sqrt 3)))
   :max-iterations 100
   :relative-tolerance 1.0e-6
   :absolute-tolerance 1.0e-6
   :line-search-relative-tolerance 1.0e-4
   :line-search-absolute-tolerance 1.0e-4
   :huber-epsilon 1.0e-4
   :quantile-p 0.5})