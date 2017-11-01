(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "Kristina Lisa Klinkner, John Alan McDonald" :date "2016-10-31"
      :doc "Iris data unit-tests." }
    
    taiga.test.classify.iris.iris
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.test.classify.iris.record :as record]))
;; mvn clean -Dtest=taiga.test.classify.iris.iris clojure:test > tests.txt
;;------------------------------------------------------------------------------
(defn options []
  (test/is (= record/attributes
              {:ground-truth record/species
               :sepal-length record/sepal-length
               :sepal-width record/sepal-width
               :petal-length record/petal-length
               :petal-width record/petal-width}))
  (let [data (record/read-tsv-file
               (io/file "data" "IrisNoSetosaBinaryData.tsv"))
        _ (test/is (== 100 (z/count data)))
        nterms 512
        mincount 1
        predictors (dissoc record/attributes :ground-truth :prediction)
        mtry (int (Math/round (Math/sqrt (z/count predictors))))
        opts {:data data :attributes record/attributes
              :nterms 512 :mincount 1 :mtry mtry}]
         (test/is (== 5 (count (:attributes opts))))
         (test/is (== 2 (:mtry opts)))
         opts))
;;------------------------------------------------------------------------------
