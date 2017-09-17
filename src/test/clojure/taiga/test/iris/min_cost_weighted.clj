(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "Kristina Lisa Klinkner, John Alan McDonald" :date "2016-11-08"
      :doc "Iris data classification forest example with min cost classification
            rule and weighted gini splits." }
    
    taiga.test.iris.min-cost-weighted
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.data.defs :as defs]
            [taiga.test.iris.record :as record]
            [taiga.test.iris.iris :as iris]))
;;  mvn -Dtest=taiga.test.iris.min-cost-weighted clojure:test
;;------------------------------------------------------------------------------
(def nss (str *ns*))
#_(test/deftest iris-min-cost-weighted-forest
  (z/reset-mersenne-twister-seeds)
  (let [options (iris/options) 
        false-negative-cost 999.0
        false-positive-cost 1.0
        options (assoc options :misclassification-costs [false-negative-cost 
                                                         false-positive-cost])
        ^clojure.lang.IFn$OD y (:ground-truth record/attributes)
        weight (fn weight ^double [datum]
                 (if (== 1.0 (.invokePrim y datum))
                   false-negative-cost
                   false-positive-cost))
        options (assoc options :weight weight)
        forest (taiga/minimum-cost-classifier options)
        _ (z/mapc #(tree/check-mincount options %) (taiga/terms forest))
        predictors (into (sorted-map)
                         (dissoc record/attributes :ground-truth :prediction))
        yhat (fn yhat ^double [datum] (forest predictors datum))]
    (test/is (= (mapv taiga/node-height (taiga/terms forest))
                [3 7 6 9 6 5 7 4 7 6 6 7 9 5 6 6 7 6 8 5 6 7 3 5 6 8 5 6 4 7 6 4
                 5 8 7 4 11 8 8 6 3 6 6 7 7 5 8 6 8 6 8 9 7 6 5 8 8 7 7 9 7 3 7 
                 5 4 5 6 6 6 8 6 8 7 9 9 7 7 10 4 6 4 6 4 7 7 8 6 7 8 4 6 8 4 4 
                 6 8 6 4 7 6 7 8 8 5 5 5 7 7 8 6 7 3 3 4 6 6 8 7 6 4 5 6 7 6 6 8
                 8 9 5 6 7 7 6 7 6 6 5 4 8 5 8 7 5 9 7 7 7 9 5 8 4 6 5 5 4 5 6 9
                 5 6 5 6 6 9 6 5 6 8 7 7 7 3 7 11 6 6 6 4 5 8 9 5 7 6 4 8 5 8 5 
                 6 4 6 6 5 6 7 7 6 7 9 4 8 5 7 6 7 9 7 7 9 3 5 5 8 8 8 3 6 5 5 9
                 8 9 7 10 9 5 5 4 5 5 6 8 6 6 7 7 7 4 10 6 7 8 6 8 8 8 7 8 5 7 6
                 6 7 6 7 7 8 8 7 5 6 7 7 7 7 7 6 3 5 9 6 6 6 8 7 7 7 5 5 9 6 7 4
                 7 5 7 7 7 7 4 4 5 6 7 8 5 6 4 6 5 5 4 4 4 6 8 5 5 7 5 6 7 7 6 8
                 8 5 3 5 5 7 6 9 6 7 6 6 7 8 7 5 7 5 6 9 6 5 5 6 5 7 4 5 5 7 5 7
                 8 10 8 3 4 7 6 5 6 6 7 6 8 5 7 5 7 8 8 5 6 6 3 7 6 5 9 2 6 10 6
                 5 7 7 8 6 8 4 8 7 3 4 4 5 6 7 9 9 6 7 7 6 7 6 7 6 6 5 5 9 7 6 6
                 7 6 7 6 9 4 9 5 5 8 8 7 6 6 4 6 7 6 7 3 5 6 6 9 5 5 8 6 7 7 8 7
                 7 6 8 4 6 3 8 5 5 3 5 7 5 6 4 6 4 6 6 5 7 9 8 7 6 7 7 5 5 6 7 6
                 5 7 2 6 7 9 8 6 7 6 5 6 7 6 5 8 9 8 7 8 4 6 7 6 8 8 7 6 5 8 6 5
                 6 5 6 9 7]))
    (test/is (= (mapv taiga/count-children (taiga/terms forest))
                [5 15 11 23 15 9 17 9 15 11 11 17 19 9 17 15 15 13 17 9 11 21 5 
                 9 11 15 11 17 7 19 15 7 13 23 25 7 25 21 21 13 5 13 17 15 15 9 
                 15 11 19 15 19 21 13 13 9 17 21 17 15 23 13 5 13 13 7 9 11 13 
                 13 15 13 19 13 19 19 17 13 19 7 13 7 15 7 13 19 19 13 19 17 7 
                 11 17 7 7 13 19 17 7 15 15 17 19 19 9 9 11 19 15 17 17 17 5 5 7
                 17 15 21 17 15 7 11 13 15 11 11 15 17 23 9 11 21 15 15 15 15 13
                 11 9 21 11 15 17 9 27 17 17 13 21 9 19 7 13 11 9 7 9 11 21 9 13
                 13 11 11 21 13 11 11 21 17 17 13 5 13 23 11 19 11 7 9 21 27 9 
                 15 15 7 23 9 17 13 11 7 11 13 9 11 19 15 13 17 27 9 21 11 15 11
                 15 17 17 15 23 5 11 9 17 17 19 5 13 11 9 21 17 19 15 21 17 9 9 
                 7 9 11 13 17 15 15 17 15 15 7 23 13 17 17 11 19 19 19 15 19 11 
                 13 15 17 13 13 17 15 21 17 17 11 17 15 15 15 15 15 13 5 13 23 
                 11 13 13 19 13 19 15 9 9 21 17 21 9 17 9 13 19 13 13 7 7 13 17 
                 13 21 9 11 9 11 11 13 7 7 9 13 17 9 11 15 9 19 17 17 15 19 17 9
                 5 9 9 15 13 21 17 13 17 15 19 17 15 13 13 11 15 21 13 11 9 13 9
                 13 7 9 13 13 13 17 15 23 17 5 9 15 11 13 11 17 13 15 17 9 17 13
                 23 21 19 13 13 15 5 17 11 9 21 3 15 21 11 11 17 17 17 15 17 9 
                 19 15 5 7 7 11 15 17 21 19 11 17 15 11 17 13 17 11 11 9 11 19 
                 15 13 13 17 11 15 13 19 7 21 11 9 21 23 19 17 15 7 15 13 11 19 
                 5 9 13 15 21 9 11 21 15 15 15 21 17 15 13 21 7 13 5 19 9 11 5 
                 11 19 11 11 7 11 7 15 11 9 15 23 17 13 11 13 19 11 11 11 17 13 
                 9 13 3 13 13 23 23 17 15 13 9 17 13 13 9 19 19 17 13 19 7 13 21
                 11 15 15 17 11 9 19 17 13 15 11 15 21 19] ))
    (test/is (= (mapv taiga/count-leaves (taiga/terms forest))
                [3 8 6 12 8 5 9 5 8 6 6 9 10 5 9 8 8 7 9 5 6 11 3 5 6 8 6 9 4 10
                 8 4 7 12 13 4 13 11 11 7 3 7 9 8 8 5 8 6 10 8 10 11 7 7 5 9 11 
                 9 8 12 7 3 7 7 4 5 6 7 7 8 7 10 7 10 10 9 7 10 4 7 4 8 4 7 10 
                 10 7 10 9 4 6 9 4 4 7 10 9 4 8 8 9 10 10 5 5 6 10 8 9 9 9 3 3 4
                 9 8 11 9 8 4 6 7 8 6 6 8 9 12 5 6 11 8 8 8 8 7 6 5 11 6 8 9 5 
                 14 9 9 7 11 5 10 4 7 6 5 4 5 6 11 5 7 7 6 6 11 7 6 6 11 9 9 7 3
                 7 12 6 10 6 4 5 11 14 5 8 8 4 12 5 9 7 6 4 6 7 5 6 10 8 7 9 14 
                 5 11 6 8 6 8 9 9 8 12 3 6 5 9 9 10 3 7 6 5 11 9 10 8 11 9 5 5 4
                 5 6 7 9 8 8 9 8 8 4 12 7 9 9 6 10 10 10 8 10 6 7 8 9 7 7 9 8 11
                 9 9 6 9 8 8 8 8 8 7 3 7 12 6 7 7 10 7 10 8 5 5 11 9 11 5 9 5 7 
                 10 7 7 4 4 7 9 7 11 5 6 5 6 6 7 4 4 5 7 9 5 6 8 5 10 9 9 8 10 9
                 5 3 5 5 8 7 11 9 7 9 8 10 9 8 7 7 6 8 11 7 6 5 7 5 7 4 5 7 7 7 
                 9 8 12 9 3 5 8 6 7 6 9 7 8 9 5 9 7 12 11 10 7 7 8 3 9 6 5 11 2 
                 8 11 6 6 9 9 9 8 9 5 10 8 3 4 4 6 8 9 11 10 6 9 8 6 9 7 9 6 6 5
                 6 10 8 7 7 9 6 8 7 10 4 11 6 5 11 12 10 9 8 4 8 7 6 10 3 5 7 8 
                 11 5 6 11 8 8 8 11 9 8 7 11 4 7 3 10 5 6 3 6 10 6 6 4 6 4 8 6 5
                 8 12 9 7 6 7 10 6 6 6 9 7 5 7 2 7 7 12 12 9 8 7 5 9 7 7 5 10 10
                 9 7 10 4 7 11 6 8 8 9 6 5 10 9 7 8 6 8 11 10]))
    (defs/serialization-test nss options forest)
    (test/is (= [26 0 24 50] (defs/print-confusion y yhat (:data options))))))
;------------------------------------------------------------------------------