(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-02-08"
      :doc "Affine data and l2 regression models." }
    
    taiga.test.regress.affine.l2
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.record :as record]
            [taiga.test.regress.data.defs :as defs]))
;; mvn -Dtest=taiga.test.regress.affine.l2 clojure:test > affine-l2.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
(z/reset-mersenne-twister-seeds)
(def options (defs/options (record/make-affine-function 10.0)))
;;----------------------------------------------------------------
(test/deftest forest
  (z/seconds 
    nss
    (let [forest (taiga/mean-regression options)
          _ (z/mapc #(tree/check-mincount options %) 
                    (taiga/terms forest))
          _ (defs/serialization-test nss options forest)
          y (:ground-truth record/attributes)
          yhat (fn yhat ^double [datum] 
                 (forest record/xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          _ (println "test:" )
          test-summary (defs/print-residual-summary 
                         y yhat (:test-data options))]
      (test/is (= (mapv taiga/node-height (taiga/terms forest))
                  [12 12 13 12 12 12 13 12 12 12 12 13 12 12 13
                   13 12 12 13 12 12 12 12 12 13 12 12 12 13 12
                   12 12 12 12 13 12 12 13 13 13 13 12 12 12 13
                   14 12 12 12 13 12 14 12 12 12 12 12 12 12 12
                   12 12 12 13 13 12 12 13 12 12 12 13 11 12 12
                   12 12 12 11 12 12 13 12 11 13 12 12 12 12 12
                   12 12 12 13 12 12 12 12 13 12 12 12 12 13 12
                   12 13 12 13 12 12 12 12 12 13 12 12 12 12 12
                   12 12 13 12 12 12 13 13]))
      (test/is (= (mapv taiga/count-children (taiga/terms forest))
                  [393 379 387 387 383 385 383 395 373 381 395 391
                   383 393 379 379 383 391 381 393 383 387 389 377
                   379 385 383 387 385 385 393 379 379 389 389 383
                   379 381 389 387 379 385 381 385 387 391 387 397
                   377 387 393 393 393 385 371 377 377 389 395 379
                   385 395 385 379 387 391 389 379 385 385 387 387
                   383 389 381 383 381 389 377 385 397 387 383 381
                   379 383 381 381 393 379 369 385 393 393 383 387
                   383 383 381 381 381 379 379 385 383 379 393 377
                   373 383 387 395 385 395 395 387 397 385 389 397
                   391 397 379 379 391 395 379 389]))
      (test/is (= (mapv taiga/count-leaves (taiga/terms forest))
                  [197 190 194 194 192 193 192 198 187 191 198 196
                   192 197 190 190 192 196 191 197 192 194 195 189
                   190 193 192 194 193 193 197 190 190 195 195 192
                   190 191 195 194 190 193 191 193 194 196 194 199
                   189 194 197 197 197 193 186 189 189 195 198 190
                   193 198 193 190 194 196 195 190 193 193 194 194
                   192 195 191 192 191 195 189 193 199 194 192 191
                   190 192 191 191 197 190 185 193 197 197 192 194
                   192 192 191 191 191 190 190 193 192 190 197 189
                   187 192 194 198 193 198 198 194 199 193 195 199
                   196 199 190 190 196 198 190 195]))
      (test/is (= [-0.012397465988435259 
                   39.53706701089951 
                   30.761653761748075] 
                  train-summary))
      (test/is (= [0.2132701307260452 
                   41.01005809672372 
                   32.205858863215305]
                  test-summary)))))
;;----------------------------------------------------------------
#_(test/deftest affine
    (z/seconds 
      nss
      (let [model (taiga/affine-regression options)
            y (:ground-truth record/attributes)
            yhat (fn yhat ^double [datum] (forest predictors datum))
            _ (println "train:" )
            train-summary (defs/print-residual-summary 
                            y yhat (:data options))
            _ (println "test:" )
            test-summary (defs/print-residual-summary 
                           y yhat (:test-data options))]
        )))
;;----------------------------------------------------------------