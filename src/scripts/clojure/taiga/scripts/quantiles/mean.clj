(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :since "2017-11-10"
      :date "2017-11-10"
      :doc "Train a mean regression forest."}
    
    taiga.scripts.quantiles.mean
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.scripts.quantiles.deciles :as deciles]
            [taiga.scripts.quantiles.record :as record]
            [taiga.scripts.quantiles.defs :as defs])
  
  (:import [clojure.lang IFn IFn$OD]
           [org.apache.commons.math3.distribution
            RealDistribution]
           [taiga.scripts.quantiles.deciles Deciles]))
;;----------------------------------------------------------------
(z/reset-mersenne-twister-seeds)
(z/seconds 
  nss
  (let [options (defs/options (record/make-pyramid-function 16.0)
                              (* 8 8 8 4 1024))
        predictors (into (sorted-map)
                         (dissoc record/attributes 
                                 :ground-truth :prediction))
        model (taiga/real-probability-measure options)
        _ (defs/serialization-test nss options model)
        predict (fn predict [datum]
                  (assoc datum 
                         :qhat (deciles/make 
                                 z/quantile 
                                 (model predictors datum))))
        train-predictions  (z/seconds 
                             "train predictions"
                             (z/map predict (:data options)))
        emp-predictions (z/seconds 
                          "emp   predictions"
                          (z/map predict (:empirical-distribution-data options)))
        test-predictions (z/seconds 
                           "test  predictions"
                           (z/map predict (:test-data options)))
        train-true-cost (z/seconds "true: " (cost record/y record/ymu train-predictions))
        train-pred-cost (z/seconds "pred: " (cost record/y record/qhat train-predictions))
        _(println "train:"
                  (float train-true-cost) 
                  (float train-pred-cost)
                  (float (/ train-pred-cost train-true-cost)))
        emp-true-cost (z/seconds "true: " (cost record/y record/ymu emp-predictions))
        emp-pred-cost (z/seconds "pred: " (cost record/y record/qhat emp-predictions))
        _(println "emp"
                  (float emp-true-cost) 
                  (float emp-pred-cost)
                  (float (/ emp-pred-cost emp-true-cost)))
        test-true-cost (z/seconds "true: " (cost record/y record/ymu test-predictions))
        test-pred-cost (z/seconds "pred: " (cost record/y record/qhat test-predictions))
        _(println "test"
                  (float test-true-cost) 
                  (float test-pred-cost)
                  (float (/ test-pred-cost test-true-cost)))]
    (z/seconds
      "train" (defs/write-predictions 
                nss options "train" model train-predictions))
    (z/seconds
      "emp" (defs/write-predictions
              nss options "emp" model emp-predictions))
    (z/seconds
      "test"(defs/write-predictions
              nss options "test" model test-predictions))
    #_(test/is (= (mapv taiga/node-height (taiga/terms forest))
                  [16 17 17 18 16 16 14 18 17 15 17 18 19 16 15 21 19 16 19 15 17 17 18 16 15 16 16 18 17 18 14 18 17 18 18 17 14 17 18 16 16 16 15 17 16 15 15 18 19 16 17 14 16 17 17 14 19 16 15 18 16 14 18 18 16 15 14 17 17 16 16 19 18 17 15 16 17 15 15 18 17 15 15 18 15 16 19 15 15 18 15 17 20 15 17 14 16 18 20 16 17 15 16 17 19 14 18 16 18 16 15 17 17 15 17 15 17 18 14 14 16 19 19 18 17 21 18 15]))
    #_(test/is (= (mapv taiga/count-children (taiga/terms forest))
                  [393 403 395 393 389 393 399 391 389 397 401 395 391 395 387 393 393 387 387 401 391 393 393 395 395 397 391 399 395 387 391 397 385 389 387 393 397 399 383 397 383 395 403 403 399 385 393 383 395 385 401 397 395 385 389 387 389 399 387 383 395 393 387 387 387 393 397 391 389 405 393 403 395 397 395 385 389 391 387 393 383 367 391 393 401 399 385 387 393 403 393 397 393 387 393 397 381 393 407 397 389 397 397 393 395 385 397 401 401 401 397 393 391 389 389 401 393 397 381 385 395 385 389 395 393 391 397 393]))
    #_(test/is (= (mapv taiga/count-leaves (taiga/terms forest))
                  [197 202 198 197 195 197 200 196 195 199 201 198 196 198 194 197 197 194 194 201 196 197 197 198 198 199 196 200 198 194 196 199 193 195 194 197 199 200 192 199 192 198 202 202 200 193 197 192 198 193 201 199 198 193 195 194 195 200 194 192 198 197 194 194 194 197 199 196 195 203 197 202 198 199 198 193 195 196 194 197 192 184 196 197 201 200 193 194 197 202 197 199 197 194 197 199 191 197 204 199 195 199 199 197 198 193 199 201 201 201 199 197 196 195 195 201 197 199 191 193 198 193 195 198 197 196 199 197])))))
;;----------------------------------------------------------------