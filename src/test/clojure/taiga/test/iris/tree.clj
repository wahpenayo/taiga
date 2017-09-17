(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "Kristina Lisa Klinkner, John Alan McDonald" :date "2016-12-22"
      :doc "Iris data classification 1-tree forest example." }
    
    taiga.test.iris.tree
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.data.defs :as defs]
            [taiga.test.iris.record :as record]
            [taiga.test.iris.iris :as iris]))
;;  mvn -Dtest=taiga.test.iris.tree clojure:test
;;------------------------------------------------------------------------------
(def nss (str *ns*))
(test/deftest iris-classification-tree
  (z/reset-mersenne-twister-seeds)
  (let [options (iris/options) 
        options (assoc options :nterms 1)
        forest (taiga/majority-vote-classifier options)
        _ (z/mapc #(tree/check-mincount options %) (taiga/terms forest))
        y (:ground-truth record/attributes)
        predictors (into (sorted-map)
                         (dissoc record/attributes :ground-truth :prediction))
        yhat (fn yhat ^double [datum] (forest predictors datum))]
    (defs/serialization-test nss options forest)
    (test/is (= (mapv taiga/node-height (taiga/terms forest)) [6]))
    (test/is (= (mapv taiga/count-children (taiga/terms forest)) [15]))
    (test/is (= (mapv taiga/count-leaves (taiga/terms forest)) [8]))
    (test/is (= [50 1 0 49] (defs/print-confusion y yhat (:data options))))))
;------------------------------------------------------------------------------