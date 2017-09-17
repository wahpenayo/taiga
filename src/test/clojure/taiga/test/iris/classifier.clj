(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "Kristina Lisa Klinkner, John Alan McDonald" :date "2016-12-22"
      :doc "Iris data classification forest example." }
    
    taiga.test.iris.classifier
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.data.defs :as defs]
            [taiga.test.iris.record :as record]
            [taiga.test.iris.iris :as iris]))
;; mvn -Dtest=taiga.test.iris.classifier clojure:test > tests.txt
;;------------------------------------------------------------------------------
(def nss (str *ns*))
(test/deftest iris-classifier-forest
  (z/reset-mersenne-twister-seeds)
  (let [options (iris/options) 
        forest (taiga/majority-vote-classifier options)
        _ (z/mapc #(tree/check-mincount options %) (taiga/terms forest))
        y (:ground-truth record/attributes)
        predictors (into (sorted-map)
                         (dissoc record/attributes :ground-truth :prediction))
        yhat (fn yhat ^double [datum] (forest predictors datum))]
    (test/is (= (mapv taiga/node-height (taiga/terms forest))
                [6 6 5 4 5 4 5 7 6 4 7 7 6 8 5 4 6 6 7 7 5 4 6 8 4 6 7 5 5 7 4 6 6 4 5 7 9 8 5 5 6 8 6 5 5 6 5 7 6 2 5 6 6 5 5 6 5 5 5 5 5 5 6 6 7 6 7 6 6 6 8 6 5 6 7 5 7 7 4 6 6 4 4 6 6 6 6 6 5 6 5 5 7 5 5 5 5 6 9 6 6 6 4 5 5 7 5 7 6 4 5 6 4 4 5 8 5 5 7 6 7 6 7 5 7 6 3 6 6 6 7 5 5 8 5 5 7 7 9 5 5 6 5 8 6 3 7 7 6 4 5 7 5 6 6 7 8 5 5 7 5 5 6 4 8 4 7 7 6 6 5 4 6 7 6 4 4 3 6 4 7 7 4 5 6 5 5 5 8 4 4 7 5 5 4 5 5 5 4 7 5 8 4 7 6 6 4 6 7 5 7 5 7 6 5 5 5 6 5 7 6 7 4 5 4 4 5 5 6 8 5 5 3 5 6 6 4 6 5 7 6 7 7 7 8 7 9 8 6 6 3 8 7 9 5 5 5 5 5 5 7 4 4 7 6 5 5 5 5 5 6 6 5 6 6 9 5 5 9 6 5 6 5 6 4 7 5 7 6 7 6 5 4 6 6 7 6 5 5 4 5 6 5 6 6 6 5 4 5 4 5 7 8 5 4 5 6 5 5 5 7 6 4 6 5 5 5 5 5 7 4 4 5 5 6 5 6 5 4 7 3 7 6 5 6 6 5 6 5 6 5 5 6 5 4 5 5 6 8 6 6 7 5 5 6 5 8 7 6 9 6 5 5 6 11 6 6 6 5 5 7 4 6 6 5 5 6 5 7 7 6 6 5 6 8 5 5 4 8 4 6 5 10 4 7 4 9 7 7 4 6 6 6 6 5 4 5 7 5 5 5 6 6 7 4 7 5 7 6 5 5 5 8 5 5 6 5 6 5 5 6 5 7 7 5 6 7 6 4 4 6 6 4 6 6 5 3 6 6 5 6 6 6 4 6 5 4 5 6 5 5 6 5 7 8 6 5 4 5 5 6 5 5 7 5 6 5 5 5 2 6 5 5 5 5 6 6 8 4 6 5 5 6 4 7 6 5 6 6 4 6 5]   ))
    (test/is (= (mapv taiga/count-children (taiga/terms forest))
                [19 21 15 9 9 9 11 19 11 7 17 13 15 23 13 13 15 11 21 21 9 11 11 27 11 21 15 15 9 19 11 15 17 11 17 27 25 25 15 15 15 19 21 13 13 21 11 15 13 3 9 17 11 11 13 15 15 19 13 15 13 11 17 13 13 15 21 13 15 15 21 17 11 21 17 9 25 17 7 15 15 7 11 17 17 15 19 15 13 19 15 15 13 15 15 17 13 11 25 15 17 19 11 13 15 15 13 19 13 11 15 19 11 7 9 21 13 11 21 11 13 15 23 13 19 17 5 11 27 19 21 9 15 25 15 9 17 19 23 11 17 21 17 21 19 5 23 23 13 7 19 15 9 19 19 23 19 11 9 23 21 19 15 9 15 9 19 15 17 11 15 7 21 21 15 9 7 5 17 7 21 21 7 17 19 11 13 11 23 11 11 19 13 15 11 11 13 15 9 17 15 19 7 27 11 21 7 21 21 13 17 11 13 13 11 13 15 19 11 17 17 13 9 13 11 7 9 17 23 15 15 15 5 9 15 15 7 21 9 23 15 15 21 19 23 23 19 21 21 17 5 19 25 23 11 11 15 15 13 11 23 7 11 13 13 15 13 13 13 11 21 21 11 21 11 19 13 11 21 17 11 13 11 17 9 23 9 15 19 21 11 17 9 15 15 15 15 9 11 9 9 11 13 13 23 23 9 7 13 13 9 17 21 11 11 9 13 11 15 15 13 13 9 11 15 11 15 9 13 19 11 11 15 19 19 15 11 9 7 13 5 21 17 13 17 11 9 17 11 17 9 9 19 15 11 15 13 11 23 13 15 27 17 13 23 17 23 19 19 29 11 13 9 17 29 13 21 11 17 13 23 11 19 15 11 11 21 11 19 15 13 17 9 15 27 9 9 11 23 7 15 13 23 11 21 7 27 17 17 7 17 19 13 17 13 9 15 15 15 11 13 19 19 23 7 15 9 21 17 13 15 13 25 11 13 15 9 15 9 19 19 13 17 23 13 17 17 15 7 11 11 19 13 11 11 19 5 23 15 11 19 15 13 9 17 17 9 15 15 13 13 11 19 25 25 15 13 13 13 15 21 15 15 17 13 21 15 13 9 3 13 15 15 17 15 11 17 23 7 19 13 15 21 7 25 15 17 29 13 9 19 13] ))
    (test/is (= (mapv taiga/count-leaves (taiga/terms forest))
                [10 11 8 5 5 5 6 10 6 4 9 7 8 12 7 7 8 6 11 11 5 6 6 14 6 11 8 8 5 10 6 8 9 6 9 14 13 13 8 8 8 10 11 7 7 11 6 8 7 2 5 9 6 6 7 8 8 10 7 8 7 6 9 7 7 8 11 7 8 8 11 9 6 11 9 5 13 9 4 8 8 4 6 9 9 8 10 8 7 10 8 8 7 8 8 9 7 6 13 8 9 10 6 7 8 8 7 10 7 6 8 10 6 4 5 11 7 6 11 6 7 8 12 7 10 9 3 6 14 10 11 5 8 13 8 5 9 10 12 6 9 11 9 11 10 3 12 12 7 4 10 8 5 10 10 12 10 6 5 12 11 10 8 5 8 5 10 8 9 6 8 4 11 11 8 5 4 3 9 4 11 11 4 9 10 6 7 6 12 6 6 10 7 8 6 6 7 8 5 9 8 10 4 14 6 11 4 11 11 7 9 6 7 7 6 7 8 10 6 9 9 7 5 7 6 4 5 9 12 8 8 8 3 5 8 8 4 11 5 12 8 8 11 10 12 12 10 11 11 9 3 10 13 12 6 6 8 8 7 6 12 4 6 7 7 8 7 7 7 6 11 11 6 11 6 10 7 6 11 9 6 7 6 9 5 12 5 8 10 11 6 9 5 8 8 8 8 5 6 5 5 6 7 7 12 12 5 4 7 7 5 9 11 6 6 5 7 6 8 8 7 7 5 6 8 6 8 5 7 10 6 6 8 10 10 8 6 5 4 7 3 11 9 7 9 6 5 9 6 9 5 5 10 8 6 8 7 6 12 7 8 14 9 7 12 9 12 10 10 15 6 7 5 9 15 7 11 6 9 7 12 6 10 8 6 6 11 6 10 8 7 9 5 8 14 5 5 6 12 4 8 7 12 6 11 4 14 9 9 4 9 10 7 9 7 5 8 8 8 6 7 10 10 12 4 8 5 11 9 7 8 7 13 6 7 8 5 8 5 10 10 7 9 12 7 9 9 8 4 6 6 10 7 6 6 10 3 12 8 6 10 8 7 5 9 9 5 8 8 7 7 6 10 13 13 8 7 7 7 8 11 8 8 9 7 11 8 7 5 2 7 8 8 9 8 6 9 12 4 10 7 8 11 4 13 8 9 15 7 5 10 7]  ))
    (defs/serialization-test nss options forest)
    (test/is (= [50 0 0 50] (defs/print-confusion y yhat (:data options))))))
;------------------------------------------------------------------------------