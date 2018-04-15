(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-14"
      :doc "data from R quantreg for regression tests." }
    
    taiga.test.regress.engel.engel
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]))
;;----------------------------------------------------------------
(z/define-datum Engel
  [^double income
   ^double foodexp
   ^double predicted-foodexp])
;;----------------------------------------------------------------
(defn options [] 
  {:data (read-tsv-file (io/file "data" "engel.csv") #",")
   :attributes {:income income
                :ground-truth foodexp
                :prediction predicted-foodexp}
   :embedding (z/linear-embedding
                "engel"
                [[:income Double/TYPE]])
   :nterms 1023
   :mincount 1
   :mtry 1
   :max-iterations 100
   :relative-tolerance 1.0e-6
   :absolute-tolerance 1.0e-6
   :line-search-relative-tolerance 1.0e-4
   :line-search-absolute-tolerance 1.0e-4
   :huber-epsilon 1.0e-6
   :quantile-p 0.5})