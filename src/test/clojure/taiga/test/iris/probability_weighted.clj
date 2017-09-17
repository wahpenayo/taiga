(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "Kristina Lisa Klinkner, John Alan McDonald" :date "2016-12-21"
      :doc "Iris data probability forest example." }
    
    taiga.test.iris.probability-weighted
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.iris.record :as record]
            [taiga.test.data.defs :as defs]
            [taiga.test.iris.iris :as iris]))
;; mvn -Dtest=taiga.test.iris.probability-weighted clojure:test > tests.txt
;;------------------------------------------------------------------------------
(def nss (str *ns*))
(test/deftest iris-probability-weighted
  (z/reset-mersenne-twister-seeds)
  (let [options (iris/options) 
        false-negative-cost 999.0
        false-positive-cost 1.0
        ^clojure.lang.IFn$OD y (:ground-truth record/attributes)
        weight (fn weight ^double [datum]
                 (if (== 1.0 (.invokePrim y datum))
                   false-negative-cost
                   false-positive-cost))
        options (assoc options :weight weight)
        forest (taiga/majority-vote-probability options)
        _ (z/mapc #(tree/check-mincount options %) (taiga/terms forest))
        predictors (into (sorted-map)
                         (dissoc record/attributes :ground-truth :prediction))
        ^clojure.lang.IFn$OD prob (fn prob ^double [datum] 
                                    (.invokePrim forest predictors datum))
        threshold (/ false-positive-cost 
                     (+ false-negative-cost false-positive-cost))
        yhat (fn yhat ^double [datum] 
               (if (< (.invokePrim prob datum) threshold) 0.0 1.0))]
    (test/is (= (mapv taiga/node-height (taiga/terms forest))
                [7 8 4 7 8 7 5 9 7 4 7 5 10 9 8 9 6 6 9 8 6 7 7 6 8 7 7 7 5 7 6 7 10 7 8 9 10 9 9 8 7 6 8 6 9 8 5 6 8 2 6 8 8 5 6 5 9 8 8 6 6 6 3 6 8 7 7 6 6 7 7 7 5 7 3 7 10 7 4 7 6 4 8 9 6 7 8 6 8 8 9 7 7 7 8 6 6 7 7 7 11 8 7 4 9 7 7 6 7 5 6 7 8 7 7 8 4 7 9 5 6 7 7 7 7 6 3 6 9 9 10 6 5 8 5 5 7 6 6 7 9 7 7 8 7 3 7 8 7 5 7 4 5 8 8 9 8 6 6 6 8 8 6 4 8 6 4 8 8 6 7 4 7 8 7 6 4 3 7 4 9 10 4 7 8 6 7 5 8 8 5 8 7 6 5 3 9 8 3 7 8 8 4 9 6 7 5 9 7 8 7 3 7 8 5 5 6 8 5 7 7 6 6 7 7 6 7 8 7 7 6 8 3 9 8 7 6 6 6 6 6 7 8 9 11 5 10 7 8 9 3 6 6 7 6 5 6 7 5 5 8 4 6 6 6 7 5 8 6 4 7 8 5 10 3 7 7 6 8 7 7 6 6 8 5 8 9 7 7 8 5 6 6 6 5 7 6 3 5 7 5 7 9 4 7 9 3 4 8 5 5 9 9 7 9 5 6 5 10 5 6 7 7 7 7 7 7 6 6 9 9 6 7 8 6 5 5 7 4 7 3 7 6 7 8 6 5 7 5 9 5 6 9 4 6 7 9 6 9 6 8 8 10 6 10 6 7 8 7 9 6 6 5 8 10 6 8 5 8 8 10 7 6 5 8 5 9 5 6 7 7 7 5 7 10 5 5 6 9 5 6 7 10 6 8 4 7 7 7 5 9 8 6 7 7 6 6 8 7 6 8 7 9 7 4 5 5 8 10 6 7 3 8 5 7 5 5 8 5 6 8 7 7 8 7 8 8 10 4 7 6 6 9 9 6 6 3 7 9 4 8 6 7 4 8 8 3 7 7 5 6 5 6 8 7 6 7 9 8 7 8 7 7 5 8 8 6 5 6 2 5 8 7 8 8 7 7 9 4 7 6 6 8 4 8 6 7 9 6 7 8 7]   ))
    (test/is (= (mapv taiga/count-children (taiga/terms forest))
                [15 15 7 13 15 15 9 19 13 7 17 11 21 19 15 21 11 11 23 17 11 13 15 15 17 17 15 13 9 23 17 17 21 15 17 23 27 21 19 19 17 23 25 13 19 17 11 11 17 3 11 17 19 11 15 9 21 19 17 15 13 11 5 13 17 15 17 13 15 13 19 13 9 17 5 13 23 17 7 17 15 7 17 25 15 21 23 15 17 23 25 13 13 15 21 13 13 13 17 15 27 23 17 7 25 17 15 11 13 9 13 17 15 17 13 21 7 19 19 9 15 17 15 15 15 17 5 11 25 21 21 11 11 23 11 9 13 17 19 15 21 17 15 23 17 5 17 19 19 9 21 9 9 19 19 27 25 13 13 13 21 19 15 7 15 13 7 15 19 11 13 7 19 21 19 11 7 5 15 7 19 21 7 19 19 15 17 11 23 19 13 21 13 13 9 5 21 15 5 17 15 17 7 23 11 17 9 23 17 17 15 5 13 21 11 11 15 15 11 15 17 11 11 17 17 11 13 21 23 15 15 19 5 19 19 15 13 13 11 13 13 15 19 17 25 11 21 15 19 27 5 13 23 15 13 11 11 15 13 9 21 7 15 13 13 15 13 19 15 7 23 17 11 21 5 17 17 13 17 13 19 17 11 21 11 19 17 17 13 23 9 11 17 13 9 15 11 5 11 13 9 15 23 7 19 21 5 7 21 11 9 23 21 17 27 9 13 11 29 9 13 15 13 17 15 15 19 11 13 21 19 19 19 25 11 9 9 13 7 13 5 15 17 19 17 11 9 17 11 17 9 15 19 7 13 17 17 11 19 13 19 21 29 15 23 15 17 17 17 25 11 15 9 17 25 13 21 9 25 17 21 13 13 11 17 11 23 11 11 15 17 15 9 15 21 9 9 15 23 9 11 15 21 15 21 7 17 15 17 9 25 19 13 15 15 13 11 15 13 11 15 25 23 15 7 9 9 15 23 13 19 5 23 11 17 9 9 19 9 15 21 15 17 23 13 21 23 21 7 17 11 15 19 17 11 13 5 23 17 7 23 11 13 9 17 21 5 19 15 13 17 9 19 17 21 15 17 23 17 17 25 17 15 13 17 19 13 9 11 3 11 27 15 17 17 15 19 25 7 15 13 15 19 7 15 13 21 21 11 13 17 15] ))
    (test/is (= (mapv taiga/count-leaves (taiga/terms forest))
                [8 8 4 7 8 8 5 10 7 4 9 6 11 10 8 11 6 6 12 9 6 7 8 8 9 9 8 7 5 12 9 9 11 8 9 12 14 11 10 10 9 12 13 7 10 9 6 6 9 2 6 9 10 6 8 5 11 10 9 8 7 6 3 7 9 8 9 7 8 7 10 7 5 9 3 7 12 9 4 9 8 4 9 13 8 11 12 8 9 12 13 7 7 8 11 7 7 7 9 8 14 12 9 4 13 9 8 6 7 5 7 9 8 9 7 11 4 10 10 5 8 9 8 8 8 9 3 6 13 11 11 6 6 12 6 5 7 9 10 8 11 9 8 12 9 3 9 10 10 5 11 5 5 10 10 14 13 7 7 7 11 10 8 4 8 7 4 8 10 6 7 4 10 11 10 6 4 3 8 4 10 11 4 10 10 8 9 6 12 10 7 11 7 7 5 3 11 8 3 9 8 9 4 12 6 9 5 12 9 9 8 3 7 11 6 6 8 8 6 8 9 6 6 9 9 6 7 11 12 8 8 10 3 10 10 8 7 7 6 7 7 8 10 9 13 6 11 8 10 14 3 7 12 8 7 6 6 8 7 5 11 4 8 7 7 8 7 10 8 4 12 9 6 11 3 9 9 7 9 7 10 9 6 11 6 10 9 9 7 12 5 6 9 7 5 8 6 3 6 7 5 8 12 4 10 11 3 4 11 6 5 12 11 9 14 5 7 6 15 5 7 8 7 9 8 8 10 6 7 11 10 10 10 13 6 5 5 7 4 7 3 8 9 10 9 6 5 9 6 9 5 8 10 4 7 9 9 6 10 7 10 11 15 8 12 8 9 9 9 13 6 8 5 9 13 7 11 5 13 9 11 7 7 6 9 6 12 6 6 8 9 8 5 8 11 5 5 8 12 5 6 8 11 8 11 4 9 8 9 5 13 10 7 8 8 7 6 8 7 6 8 13 12 8 4 5 5 8 12 7 10 3 12 6 9 5 5 10 5 8 11 8 9 12 7 11 12 11 4 9 6 8 10 9 6 7 3 12 9 4 12 6 7 5 9 11 3 10 8 7 9 5 10 9 11 8 9 12 9 9 13 9 8 7 9 10 7 5 6 2 6 14 8 9 9 8 10 13 4 8 7 8 10 4 8 7 11 11 6 7 9 8]   ))
    (defs/serialization-test nss options forest)
    (test/is (= [33 0 17 50] (defs/print-confusion y yhat (:data options))))))
;;------------------------------------------------------------------------------