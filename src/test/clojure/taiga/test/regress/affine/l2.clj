(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-02-10"
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
;;----------------------------------------------------------------
#_(test/deftest noiseless-affine
   (z/reset-mersenne-twister-seeds)
   (z/seconds 
     nss
     (let [options (defs/options 
                     record/attributes
                     record/xbindings
                     record/generator
                     (record/make-affine-function 10.0)
                     -1.0)
           model (taiga/affine-l2-regression options)
           ;;_ (defs/serialization-test nss options model)
           y (:ground-truth record/attributes)
           yhat (fn yhat ^double [datum] 
                  (model record/xbindings datum))
           _ (println "train:" )
           train-summary (defs/print-residual-summary 
                           y yhat (:data options))
           _ (println "test:" )
           test-summary (defs/print-residual-summary 
                          y yhat (:test-data options))]
       (test/is (= [-1.7604732775733378E-17 
                    8.841986127560332E-17 
                    1.7604732775733378E-17]
                   train-summary))
       (test/is (= [-1.8241701552068612E-17 
                    9.000523778413571E-17 
                    1.8241701552068612E-17]
                   test-summary)))))
;;----------------------------------------------------------------
#_(test/deftest affine
   (z/reset-mersenne-twister-seeds)
   (z/seconds 
     nss
     (let [options (defs/options 
                     record/attributes
                     record/xbindings
                     record/generator
                     (record/make-affine-function 10.0)
                     2.0)
           model (taiga/affine-l2-regression options)
           ;;_ (defs/serialization-test nss options model)
           y (:ground-truth record/attributes)
           yhat (fn yhat ^double [datum] 
                  (model record/xbindings datum))
           _ (println "train:" )
           train-summary (defs/print-residual-summary 
                           y yhat (:data options))
           _ (println "test:" )
           test-summary (defs/print-residual-summary 
                          y yhat (:test-data options))]
       (test/is (= [-1.7604732775733378E-17 
                    8.841986127560332E-17 
                    1.7604732775733378E-17]
                   train-summary))
       (test/is (= [-1.8241701552068612E-17 
                    9.000523778413571E-17 
                    1.8241701552068612E-17]
                   test-summary)))))
;;----------------------------------------------------------------
(test/deftest noiseless-forest
  (z/reset-mersenne-twister-seeds)
  (z/seconds 
    nss
    (let [options (defs/options 
                    record/attributes
                    record/xbindings
                    record/generator
                    (record/make-affine-function 10.0)
                    -1.0)
          model (taiga/mean-regression options)
          _ (z/mapc #(tree/check-mincount options %) 
                    (taiga/terms model))
          _ (defs/serialization-test nss options model)
          y (:ground-truth record/attributes)
          yhat (fn yhat ^double [datum] 
                 (model record/xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          _ (println "test:" )
          test-summary (defs/print-residual-summary 
                         y yhat (:test-data options))]
      (test/is (= (mapv taiga/node-height (taiga/terms model))
                  [12 12 13 12 12 13 13 12 12 12 12 12 12 12 12 13
                   12 12 13 12 12 12 12 12 13 12 12 12 13 12 12 12
                   12 12 12 12 12 11 12 13 13 12 12 12 13 14 12 12
                   12 13 12 13 13 12 12 12 12 12 12 12 12 12 12 13
                   13 12 12 12 12 12 12 13 11 12 12 12 12 12 12 12
                   12 13 12 11 13 12 12 13 12 12 12 12 12 13 12 12
                   12 12 13 12 12 12 12 13 12 12 12 12 12 12 12 12
                   12 12 13 12 12 13 12 12 12 12 13 12 12 12 13 12]))
      (test/is (= (mapv taiga/count-children (taiga/terms model))
                  [393 379 387 381 387 385 393 395 377 387 387 393
                   387 393 381 381 383 393 381 393 383 389 389 385
                   393 383 383 391 385 383 393 389 373 391 391 395
                   379 369 393 387 379 385 381 385 387 391 381 377
                   377 397 393 391 389 385 379 377 373 389 389 387
                   385 395 389 379 387 391 389 379 385 377 389 383
                   383 385 381 383 385 375 387 383 397 387 383 375
                   379 381 389 393 387 379 369 385 387 391 381 391
                   385 383 381 381 381 375 397 387 381 379 387 377
                   381 383 379 395 385 395 395 387 397 375 389 375
                   391 397 373 387 391 391 379 391]))
      (test/is (= (mapv taiga/count-leaves (taiga/terms model))
                  [197 190 194 191 194 193 197 198 189 194 194 197
                   194 197 191 191 192 197 191 197 192 195 195 193
                   197 192 192 196 193 192 197 195 187 196 196 198
                   190 185 197 194 190 193 191 193 194 196 191 189
                   189 199 197 196 195 193 190 189 187 195 195 194
                   193 198 195 190 194 196 195 190 193 189 195 192
                   192 193 191 192 193 188 194 192 199 194 192 188
                   190 191 195 197 194 190 185 193 194 196 191 196
                   193 192 191 191 191 188 199 194 191 190 194 189
                   191 192 190 198 193 198 198 194 199 188 195 188
                   196 199 187 194 196 196 190 196]))
      (test/is (= [-0.007654331308799542
                   39.631106028619286
                   30.8506863878784265] 
                  train-summary))
      (test/is (= [0.19415302498667997
                   41.14836534172385
                   32.34920275535752]
                  test-summary)))))
;;----------------------------------------------------------------
(test/deftest forest
  (z/reset-mersenne-twister-seeds)
  (z/seconds 
    nss
    (let [options (defs/options 
                    record/attributes
                    record/xbindings
                    record/generator
                    (record/make-affine-function 10.0)
                    2.0)
          model (taiga/mean-regression options)
          _ (z/mapc #(tree/check-mincount options %) 
                    (taiga/terms model))
          _ (defs/serialization-test nss options model)
          y (:ground-truth record/attributes)
          yhat (fn yhat ^double [datum] 
                 (model record/xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          _ (println "test:" )
          test-summary (defs/print-residual-summary 
                         y yhat (:test-data options))]
      (test/is (= (mapv taiga/node-height (taiga/terms model))
                  [12 12 13 12 12 12 13 12 12 12 12 13 12 12 13
                   13 12 12 13 12 12 12 12 12 13 12 12 12 13 12
                   12 12 12 12 13 12 12 13 13 13 13 12 12 12 13
                   14 12 12 12 13 12 14 12 12 12 12 12 12 12 12
                   12 12 12 13 13 12 12 13 12 12 12 13 11 12 12
                   12 12 12 11 12 12 13 12 11 13 12 12 12 12 12
                   12 12 12 13 12 12 12 12 13 12 12 12 12 13 12
                   12 13 12 13 12 12 12 12 12 13 12 12 12 12 12
                   12 12 13 12 12 12 13 13]))
      (test/is (= (mapv taiga/count-children (taiga/terms model))
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
      (test/is (= (mapv taiga/count-leaves (taiga/terms model))
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
            yhat (fn yhat ^double [datum] (model predictors datum))
            _ (println "train:" )
            train-summary (defs/print-residual-summary 
                            y yhat (:data options))
            _ (println "test:" )
            test-summary (defs/print-residual-summary 
                           y yhat (:test-data options))]
        )))
;;----------------------------------------------------------------