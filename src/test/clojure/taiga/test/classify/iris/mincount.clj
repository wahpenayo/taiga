(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "Kristina Lisa Klinkner, John Alan McDonald" :date "2016-11-08"
      :doc "Iris data probability forest example." }
    
    taiga.test.classify.iris.mincount
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.classify.data.defs :as defs]
            [taiga.test.classify.iris.record :as record]
            [taiga.test.classify.iris.iris :as iris]))
;; mvn -Dtest=taiga.test.classify.iris.mincount clojure:test  > tests.txt
;;------------------------------------------------------------------------------
(def nss (str *ns*))
(test/deftest iris-mincount
  (z/reset-mersenne-twister-seeds)
  (let [options (iris/options) 
        options (assoc options :mincount 15)
        false-negative-cost 999.0
        false-positive-cost 1.0
        ^clojure.lang.IFn$OD y (:ground-truth record/attributes)
        weight (fn weight ^double [datum]
                 (if (== 1.0 (.invokePrim y datum))
                   false-negative-cost
                   false-positive-cost))
        options (assoc options :weight weight)           
        predictors (into (sorted-map)
                         (dissoc record/attributes :ground-truth :prediction))
        forest (taiga/majority-vote-probability options)
        ^clojure.lang.IFn$OD prob (fn prob ^double [datum] 
                                    (.invokePrim forest predictors datum))
        threshold (/ false-positive-cost 
                     (+ false-negative-cost false-positive-cost))
        yhat (fn yhat ^double [datum] 
               (if (< (.invokePrim prob datum) threshold) 0.0 1.0))]
    (z/mapc #(tree/check-mincount options %) (taiga/terms forest))
    (test/is (= (mapv taiga/node-height (taiga/terms forest))
                 [4 4 3 4 3 4 3 4 3 3 4 3 5 3 3 4 4 3 4 3 3 3 4 4 4 5 4 5 3 4 4 
                  4 4 4 5 4 4 4 4 5 4 4 4 4 4 4 4 3 4 2 3 4 4 3 4 3 4 5 4 4 4 3 
                  3 3 3 3 4 3 4 3 4 3 3 4 3 3 4 3 3 4 3 3 4 4 4 4 5 3 4 4 4 3 4 
                  4 3 4 4 3 4 4 3 4 4 3 4 4 4 4 3 4 4 4 3 3 3 4 3 4 4 3 3 4 3 3 
                  4 4 3 3 4 5 4 3 3 4 4 3 4 4 4 4 4 4 3 4 3 3 4 4 4 3 4 3 3 4 4 
                  5 4 3 3 4 4 4 3 3 3 4 3 4 4 3 4 3 4 4 3 4 4 3 4 3 5 4 3 4 4 3 
                  4 3 4 4 3 4 3 4 3 3 4 3 4 4 4 3 3 5 4 4 3 3 4 3 4 3 3 4 3 4 4 
                  3 4 4 3 4 4 3 3 3 3 3 4 4 4 4 3 3 4 3 3 3 3 4 3 3 4 3 4 5 3 3 
                  5 4 3 4 4 5 3 3 3 4 4 4 4 4 4 3 3 4 3 4 4 3 4 5 3 5 3 3 4 3 4 
                  3 3 4 3 4 4 4 3 4 5 4 4 4 4 3 4 3 4 3 3 4 3 3 4 4 4 4 3 3 5 4 
                  4 3 5 4 4 3 3 4 4 3 4 3 3 4 4 3 4 3 4 5 4 4 4 4 4 4 3 3 3 3 3 
                  4 4 4 5 3 3 3 3 4 3 3 3 4 4 4 4 3 4 3 4 5 4 4 5 4 4 4 4 4 3 4 
                  3 4 4 3 4 3 3 4 4 3 3 4 3 3 4 4 4 4 4 3 3 4 4 4 4 5 4 3 5 4 4 
                  3 5 3 4 3 4 3 4 5 3 4 3 4 3 4 3 3 3 4 5 5 3 3 3 4 4 4 4 3 4 3 
                  4 3 3 5 3 4 4 4 3 4 4 4 3 4 3 3 3 4 4 3 3 3 3 4 4 4 4 5 3 3 3 
                  4 3 4 4 3 4 3 4 4 3 4 3 4 3 4 4 4 4 4 4 3 3 3 4 2 3 4 5 4 3 3 
                  4 5 3 4 4 4 5 3 4 3 4 4 3 3 4 4] ))
    (test/is (= (mapv taiga/count-children (taiga/terms forest))
                [7 7 5 7 5 7 5 7 5 5 7 5 9 7 5 7 7 5 7 5 5 5 7 7 7 9 7 9 5 7 7 7
                 7 7 11 7 7 9 7 9 7 7 7 7 7 7 7 5 7 3 5 7 7 5 7 7 9 9 7 7 7 5 5 
                 5 5 5 7 5 7 5 7 5 5 9 5 5 7 5 5 7 5 5 7 7 7 7 9 5 7 7 7 5 7 7 5
                 7 7 5 7 7 7 7 7 5 7 7 7 7 7 7 7 7 5 5 5 7 5 7 7 5 5 7 7 5 7 7 5
                 5 7 9 7 5 7 7 7 5 7 7 7 7 7 7 5 9 7 5 7 7 7 5 7 5 5 7 7 9 7 5 5
                 7 7 7 5 5 5 7 5 7 7 5 7 5 7 7 5 7 7 5 7 5 9 9 5 7 7 5 7 5 7 9 5
                 7 5 7 5 5 7 5 7 7 7 5 5 9 7 7 5 7 7 5 7 5 5 7 5 7 7 5 7 7 7 7 7
                 5 5 5 5 7 7 7 7 9 5 5 7 5 5 5 5 7 5 5 9 5 7 9 5 5 9 7 5 7 7 9 7
                 5 5 7 7 7 7 7 7 5 5 7 5 7 7 5 9 9 5 9 5 5 7 5 7 5 5 7 5 7 7 7 5
                 7 9 7 7 9 7 5 7 5 7 5 5 7 5 5 7 7 7 7 5 5 9 7 7 5 9 7 7 5 5 7 7
                 5 7 5 5 7 7 5 7 5 7 9 7 7 7 7 7 9 5 5 5 5 5 9 7 7 9 5 5 5 5 7 5
                 5 5 7 7 7 7 5 7 5 7 9 7 7 9 7 7 7 7 7 5 7 5 7 7 5 7 5 5 7 7 5 5 
                 7 5 5 7 7 7 7 7 5 5 7 9 7 7 9 9 5 9 7 7 5 9 5 7 5 7 5 7 9 5 7 5 
                 9 5 7 5 5 5 7 9 9 5 5 5 7 7 9 7 5 7 5 7 5 5 9 5 7 7 7 5 7 7 7 5
                 7 5 5 5 7 7 5 5 5 5 7 7 7 7 9 5 5 5 7 7 7 7 5 7 5 7 7 5 7 5 7 5 
                 7 7 7 7 7 7 7 5 5 7 3 5 9 9 7 5 5 7 9 5 7 7 7 9 5 9 5 7 7 5 5 7 
                 7] ))
    (test/is (= (mapv taiga/count-leaves (taiga/terms forest))
                [4 4 3 4 3 4 3 4 3 3 4 3 5 4 3 4 4 3 4 3 3 3 4 4 4 5 4 5 3 4 4 4
                 4 4 6 4 4 5 4 5 4 4 4 4 4 4 4 3 4 2 3 4 4 3 4 4 5 5 4 4 4 3 3 3 
                 3 3 4 3 4 3 4 3 3 5 3 3 4 3 3 4 3 3 4 4 4 4 5 3 4 4 4 3 4 4 3 4 
                 4 3 4 4 4 4 4 3 4 4 4 4 4 4 4 4 3 3 3 4 3 4 4 3 3 4 4 3 4 4 3 3 
                 4 5 4 3 4 4 4 3 4 4 4 4 4 4 3 5 4 3 4 4 4 3 4 3 3 4 4 5 4 3 3 4 
                 4 4 3 3 3 4 3 4 4 3 4 3 4 4 3 4 4 3 4 3 5 5 3 4 4 3 4 3 4 5 3 4 
                 3 4 3 3 4 3 4 4 4 3 3 5 4 4 3 4 4 3 4 3 3 4 3 4 4 3 4 4 4 4 4 3 
                 3 3 3 4 4 4 4 5 3 3 4 3 3 3 3 4 3 3 5 3 4 5 3 3 5 4 3 4 4 5 4 3 
                 3 4 4 4 4 4 4 3 3 4 3 4 4 3 5 5 3 5 3 3 4 3 4 3 3 4 3 4 4 4 3 4 
                 5 4 4 5 4 3 4 3 4 3 3 4 3 3 4 4 4 4 3 3 5 4 4 3 5 4 4 3 3 4 4 3 
                 4 3 3 4 4 3 4 3 4 5 4 4 4 4 4 5 3 3 3 3 3 5 4 4 5 3 3 3 3 4 3 3 
                 3 4 4 4 4 3 4 3 4 5 4 4 5 4 4 4 4 4 3 4 3 4 4 3 4 3 3 4 4 3 3 4 
                 3 3 4 4 4 4 4 3 3 4 5 4 4 5 5 3 5 4 4 3 5 3 4 3 4 3 4 5 3 4 3 5 
                 3 4 3 3 3 4 5 5 3 3 3 4 4 5 4 3 4 3 4 3 3 5 3 4 4 4 3 4 4 4 3 4 
                 3 3 3 4 4 3 3 3 3 4 4 4 4 5 3 3 3 4 4 4 4 3 4 3 4 4 3 4 3 4 3 4 
                 4 4 4 4 4 4 3 3 4 2 3 5 5 4 3 3 4 5 3 4 4 4 5 3 5 3 4 4 3 3 4 4
                 ]  ))
    (defs/serialization-test nss options forest)
    (test/is (= [0 0 50 50] (defs/print-confusion y yhat (:data options))))))
;;------------------------------------------------------------------------------