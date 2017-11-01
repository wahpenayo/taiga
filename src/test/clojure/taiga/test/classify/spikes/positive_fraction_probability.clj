(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-12-22"
      :doc "Spikes function probability forest example." }
    
    taiga.test.classify.spikes.positive-fraction-probability
  
  (:require [clojure.pprint :as pp]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.classify.data.record :as record]
            [taiga.test.classify.data.defs :as defs]))
;; mvn -Dtest=taiga.test.classify.spikes.positive-fraction-probability clojure:test > tests.txt
;;------------------------------------------------------------------------------
(def nss (str *ns*))
(test/deftest spikes-positive-fraction-probability
  (z/seconds 
    nss
    (z/reset-mersenne-twister-seeds)
    (let [options (defs/options (record/make-spikes-function 1.0))
          train-freqs (z/frequencies record/true-class (:data options))
          _ (println "train" train-freqs (/ (double (z/get train-freqs 1.0)) 
                                            (z/count (:data options)))) 
          test-freqs (z/frequencies record/true-class (:test-data options))
          _ (println "test" test-freqs (/ (double (z/get test-freqs 1.0) )
                                          (z/count (:test-data options))))  
          forest (taiga/positive-fraction-probability options)
          _ (z/mapc #(tree/check-mincount options %) (taiga/terms forest))
          ;;_ (defs/serialization-test nss options forest)
          attributes (assoc record/attributes
                            :ground-truth record/true-probability
                            :prediction record/predicted-probability)
          predictors (into (sorted-map)
                           (dissoc attributes :ground-truth :prediction))
          ^clojure.lang.IFn$OD p record/true-probability
          ^clojure.lang.IFn$OD phat (fn phat ^double [datum] 
                                      (.invokePrim forest predictors datum))
          ^clojure.lang.IFn$OD chat (fn chat ^double [datum] 
                                      (if (< (.invokePrim phat datum) 0.5) 0.0 1.0))
          train-confusion (defs/confusion record/true-class chat (:data options))
          train-confusion-rate (defs/confusion-rate train-confusion)
          train-mad (z/mean-absolute-difference p phat (:data options))
          test-confusion (defs/confusion record/true-class chat (:test-data options))
          test-confusion-rate (defs/confusion-rate test-confusion)
          test-mad (z/mean-absolute-difference p phat (:test-data options))
          metrics {:mad taiga/mean-absolute-error :rmse taiga/rms-error}]
      (println "train:") (println train-confusion-rate) (println train-confusion)
      (test/is (== (float train-confusion-rate) (float 0.045867919921875)))
      (test/is (= [31265 1503 0 0] train-confusion))
      (test/is (== (z/count (:data options)) (reduce + train-confusion)))
      (println "test:") (println test-confusion-rate) (println test-confusion)
      (test/is (== (float test-confusion-rate) (float 0.04827880859375)))
      (test/is (= [31186 1582 0 0] test-confusion))
      (test/is (== (z/count (:test-data options)) (reduce + test-confusion)))
      (test/is (= (mapv taiga/node-height (taiga/terms forest))
                  [18 16 13 16 15 13 15 14 14 15 14 15 14 15 15 14 16 13 14 15 13 17 16 14 15 15 14 15 15 13 15 14 12 16 15 15 16 14 15 15 15 15 15 16 14 14 15 16 13 14 16 16 15 16 17 14 15 18 16 14 16 13 14 14 17 13 15 14 15 15 14 16 17 14 17 17 14 14 20 15 13 14 14 14 17 13 14 16 13 18 15 14 12 16 14 14 17 18 14 14 15 14 14 14 14 18 20 15 14 16 13 15 15 14 13 15 13 16 12 15 13 15 14 13 17 19 16 15]     ))
      (test/is (= (mapv taiga/count-children (taiga/terms forest))
                  [183 187 197 211 209 191 197 175 191 233 191 185 205 209 209 203 227 203 189 197 195 197 195 187 213 205 227 225 199 217 205 209 199 201 225 215 207 207 217 219 227 213 189 195 221 209 227 195 213 193 235 215 207 231 215 213 233 201 195 225 227 191 193 217 213 197 203 213 205 189 211 195 211 197 227 213 201 227 215 223 209 207 215 203 207 195 217 205 211 233 221 185 169 233 191 217 207 225 201 179 201 237 211 205 205 243 211 183 193 201 219 215 207 217 197 211 209 209 207 231 203 229 193 193 199 215 211 201]      ))
      (test/is (= (mapv taiga/count-leaves (taiga/terms forest))
                  [92 94 99 106 105 96 99 88 96 117 96 93 103 105 105 102 114 102 95 99 98 99 98 94 107 103 114 113 100 109 103 105 100 101 113 108 104 104 109 110 114 107 95 98 111 105 114 98 107 97 118 108 104 116 108 107 117 101 98 113 114 96 97 109 107 99 102 107 103 95 106 98 106 99 114 107 101 114 108 112 105 104 108 102 104 98 109 103 106 117 111 93 85 117 96 109 104 113 101 90 101 119 106 103 103 122 106 92 97 101 110 108 104 109 99 106 105 105 104 116 102 115 97 97 100 108 106 101]     ))
      (println "train" train-mad)
      (test/is (== (float train-mad) (float 0.03144772226810712)))
      (println "test" test-mad)
      (test/is (== (float test-mad) (float 0.033051658940432944)))
      (println "train importance:")
      (pp/pprint (taiga/permutation-statistics 
                   metrics forest attributes (:data options)))
      (println "test importance:")
      (pp/pprint (taiga/permutation-statistics 
                   metrics forest attributes (:test-data options)))
      (defs/write-predictions 
        nss options "train" forest
        (defs/predictions phat (:data options) :predicted-probability))
      (defs/write-predictions 
        nss options "test"  forest
        (defs/predictions phat (:test-data options) :predicted-probability))
      #_(defs/by-nterms nss options forest predictors))))
;;------------------------------------------------------------------------------
