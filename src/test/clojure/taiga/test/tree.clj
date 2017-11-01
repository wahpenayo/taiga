(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-11-11"
      :doc "Check tree properties." }
    
    taiga.test.tree
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.tree.node :as node]
            [taiga.test.classify.data.record :as record]
            [taiga.test.classify.data.defs :as defs]))
;;  mvn -Dtest=taiga.test.leaves clojure:test
;;------------------------------------------------------------------------------
(defn check-mincount 
  "This is a stochastic test --- we are checking how many cases from the original
   data end up in each leaf, not how many in the bag that was used to train the 
   tree."
  [options tree]
  (let [{:keys [data ^long mincount attributes]} options
        threshold (quot (* 2 mincount) 3)
        predictors (dissoc attributes :ground-truth :prediction)
        leaf->data (z/group-by #(node/leaf tree predictors %) data)]
    (test/is (== (z/count data) (reduce + (map z/count (z/vals leaf->data)))))
    #_(z/mapc
      #(test/is (<= threshold (z/count (z/get leaf->data %))))
      (z/keys leaf->data))))
;;------------------------------------------------------------------------------
