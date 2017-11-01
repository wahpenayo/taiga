(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-12-22"
      :doc "Step function probability forest example." }
    
    taiga.test.classify.step.probability
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.classify.data.record :as record]
            [taiga.test.classify.data.defs :as defs]))
;; mvn clean install -DskipTests=true
;; mvn -Dtest=taiga.test.classify.step.probability clojure:test > tests.txt
;;------------------------------------------------------------------------------
(def nss (str *ns*))
(test/deftest step-probability
  (z/reset-mersenne-twister-seeds)
  (let [options (defs/options (record/make-diagonal-step-function 1.0))
        forest (taiga/majority-vote-probability options)
        _ (z/mapc #(tree/check-mincount options %) (taiga/terms forest))
        ;;_ (defs/serialization-test nss options forest)
        predictors (into (sorted-map)
                         (dissoc record/attributes :ground-truth :prediction))]
    (test/is (= (mapv taiga/node-height (taiga/terms forest))
                [11 11 12 19 9 15 17 9 18 14 11 13 11 16 14 10 14 11 8 13 11 14 18 14 13 20 9 13 15 8 15 10 12 11 7 17 11 12 8 11 10 14 7 8 18 13 13 10 11 7 14 17 15 14 12 13 16 10 11 16 16 20 17 7 10 17 9 14 10 14 19 7 11 16 6 12 14 9 14 10 14 10 8 12 10 10 15 9 11 12 16 12 13 10 14 7 20 14 13 13 14 14 10 16 11 9 9 13 8 8 19 15 15 9 11 9 16 10 15 9 11 13 11 9 11 10 11 14]   ))
    (test/is (= (mapv taiga/count-children (taiga/terms forest))
                [87 113 91 229 35 145 99 45 157 151 151 75 85 201 197 49 83 79 49 159 47 117 125 117 113 165 69 83 179 61 183 55 39 47 29 151 99 129 29 63 55 135 35 43 151 85 149 71 93 29 105 223 139 131 125 61 141 127 77 131 181 177 207 33 67 187 67 133 69 229 153 21 91 221 25 121 171 31 147 79 157 61 31 103 49 59 191 57 111 63 199 111 133 103 197 43 213 107 149 73 193 147 67 147 115 47 29 151 21 55 147 147 137 93 115 47 147 61 151 25 109 57 129 43 103 57 137 113]   ))
    (test/is (= (mapv taiga/count-leaves (taiga/terms forest))
                [44 57 46 115 18 73 50 23 79 76 76 38 43 101 99 25 42 40 25 80 24 59 63 59 57 83 35 42 90 31 92 28 20 24 15 76 50 65 15 32 28 68 18 22 76 43 75 36 47 15 53 112 70 66 63 31 71 64 39 66 91 89 104 17 34 94 34 67 35 115 77 11 46 111 13 61 86 16 74 40 79 31 16 52 25 30 96 29 56 32 100 56 67 52 99 22 107 54 75 37 97 74 34 74 58 24 15 76 11 28 74 74 69 47 58 24 74 31 76 13 55 29 65 22 52 29 69 57]           ))
    (defs/write-predictions 
      nss options "train" forest
      (defs/predictions 
        forest predictors (:data options) :predicted-probability))
    (defs/write-predictions 
      nss options "test" forest
      (defs/predictions 
        forest predictors (:test-data options) :predicted-probability))
    #_(defs/by-nterms nss options forest predictors)))
;;------------------------------------------------------------------------------