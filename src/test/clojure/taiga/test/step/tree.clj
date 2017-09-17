(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-12-22"
      :doc "Step function probability forest example." }
    
    taiga.test.step.tree
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.data.record :as record]
            [taiga.test.data.defs :as defs]))
;; mvn -Dtest=taiga.test.step.tree clojure:test > tests.txt
;;------------------------------------------------------------------------------
(def nss (str *ns*))
(test/deftest step-tree
  (z/reset-mersenne-twister-seeds)
  (let [options (defs/options (record/make-diagonal-step-function 1.0))
        options (assoc options :nterms 1)
        forest (taiga/majority-vote-probability options)
        predictors (into (sorted-map)
                         (dissoc record/attributes :ground-truth :prediction))
        ^clojure.lang.IFn$OD p record/true-probability
        ^clojure.lang.IFn$OD phat (fn phat ^double [datum] 
                                    (.invokePrim forest predictors datum))
        _ (println "train MAD:" 
                   (z/mean-absolute-difference p phat (:data options)))
        _ (println "test MAD:" 
                   (z/mean-absolute-difference p phat (:test-data options)))
        y record/true-class
        yhat (fn yhat ^double [datum] 
               (if (< (.invokePrim phat datum) 0.5) 0.0 1.0))
        _ (println "train:" )
        train-confusion (defs/print-confusion y yhat (:data options))
        _ (println "test:" )
        test-confusion (defs/print-confusion y yhat (:test-data options))]
    (defs/serialization-test nss options forest)
    (z/mapc #(tree/check-mincount options %) (taiga/terms forest))
    (test/is (== (z/count (:data options)) (reduce + train-confusion)))
    (test/is (= [14635 1776 1985 14372] train-confusion))
    (test/is (== (z/count (:test-data options)) (reduce + test-confusion)))
    (test/is (= [14338 1874 2090 14466] test-confusion))
    (test/is (= (mapv taiga/node-height (taiga/terms forest)) [19]))
    (test/is (= (mapv taiga/count-children (taiga/terms forest)) [203]))
    (test/is (= (mapv taiga/count-leaves (taiga/terms forest)) [102]))))
;------------------------------------------------------------------------------