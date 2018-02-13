(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-02-12"
      :doc "Pyramid data and l2 regression models." }
    
    taiga.test.regress.pyramid.l2
  
  (:require [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.record :as record]
            [taiga.test.regress.data.defs :as defs]))
;; mvn -Dtest=taiga.test.regress.pyramid.l2 clojure:test > pyramid-l2.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
;;----------------------------------------------------------------
(test/deftest noiseless-forest
  (z/seconds 
    nss
    (z/reset-mersenne-twister-seeds)
    (let [options (defs/options 
                    record/attributes
                    record/xbindings
                    record/generator
                    (record/make-pyramid-function 10.0)
                    -1.0)
          model (taiga/mean-regression options)
          _ (z/mapc #(tree/check-mincount options %) 
                    (taiga/terms model))
          _ (defs/json-test nss options model)
          _ (defs/edn-test 
              model (defs/forest-file nss options model))
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
                  [13 12 12 18 13 11 13 16 14 16 12 17 17 13 14 14
                   15 16 14 13 13 18 13 16 14 13 11 14 14 12 15 12
                   16 13 13 13 15 12 15 14 11 13 12 13 12 12 14 16 
                   13 16 16 12 14 16 15 17 13 13 17 16 14 13 14 15
                   13 13 15 16 12 13 11 13 13 14 16 14 11 15 13 12
                   16 12 12 14 12 15 13 11 12 15 21 13 12 18 12 12
                   13 13 15 12 13 15 12 13 13 14 14 15 18 11 13 18
                   12 14 13 11 13 19 13 14 12 12 11 12 13 13 16 13]))
      (test/is (= (mapv taiga/count-children (taiga/terms model))
                  [379 389 381 387 389 391 387 393 383 391 375 385
                   389 395 381 387 379 395 385 379 381 385 367 393
                   387 381 391 377 387 379 379 393 387 365 387 377
                   393 391 383 383 385 385 399 385 381 385 379 387
                   375 385 373 387 387 385 383 385 375 395 371 375
                   385 395 383 391 393 381 381 385 391 379 389 393
                   379 387 391 381 379 395 387 395 393 389 385 381
                   385 377 389 405 373 389 375 373 381 401 375 395
                   391 381 385 393 391 381 369 377 375 387 385 393
                   391 393 391 389 377 387 383 383 385 381 385 387
                   399 387 375 395 389 387 385 391]))
      (test/is (= (mapv taiga/count-leaves (taiga/terms model))
                  [190 195 191 194 195 196 194 197 192 196 188 193
                   195 198 191 194 190 198 193 190 191 193 184 197
                   194 191 196 189 194 190 190 197 194 183 194 189
                   197 196 192 192 193 193 200 193 191 193 190 194
                   188 193 187 194 194 193 192 193 188 198 186 188
                   193 198 192 196 197 191 191 193 196 190 195 197
                   190 194 196 191 190 198 194 198 197 195 193 191
                   193 189 195 203 187 195 188 187 191 201 188 198
                   196 191 193 197 196 191 185 189 188 194 193 197
                   196 197 196 195 189 194 192 192 193 191 193 194
                   200 194 188 198 195 194 193 196]))
      (test/is 
        (= [-9.212508747065476E-4
            1.021415242778652
            0.7281127341594941] 
           train-summary))
      (test/is 
        (= [-3.0943854765225356E-4
            1.0658733374339389
            0.7610244066836168]
           test-summary)))))
;;----------------------------------------------------------------
(test/deftest forest
  (z/seconds 
    nss
    (z/reset-mersenne-twister-seeds)
    (let [options (defs/options 
                    record/attributes
                    record/xbindings
                    record/generator
                    (record/make-pyramid-function 10.0)
                    2.0)
          model (taiga/mean-regression options)
          _ (z/mapc #(tree/check-mincount options %) 
                    (taiga/terms model))
          _ (defs/json-test nss options model)
          _ (defs/edn-test 
              model (defs/forest-file nss options model))
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
                  [11 12 12 16 14 13 11 15 13 18 13 14 16 12 15 13
                   14 15 14 12 11 15 16 17 14 14 13 14 16 13 17 13
                   19 19 13 14 12 12 16 15 12 15 11 12 12 14 13 13
                   14 12 20 13 13 14 15 17 13 12 13 13 13 14 13 16
                   13 13 17 15 13 13 12 17 11 13 16 14 13 15 12 11
                   16 13 12 15 11 12 13 12 11 16 20 16 14 17 12 12
                   13 13 13 11 11 14 14 12 14 12 13 14 18 12 14 13 
                   14 15 15 12 14 20 13 12 13 15 12 12 13 14 15 14]))
      (test/is (= (mapv taiga/count-children (taiga/terms model))
                  [391 389 385 393 381 387 391 385 379 381 379 395
                   381 383 393 391 393 385 387 389 385 375 387 397
                   377 397 387 377 397 391 375 383 387 389 389 387
                   387 393 381 385 389 375 385 381 383 367 385 393
                   379 391 385 391 385 379 401 383 391 379 393 383
                   375 375 387 385 387 371 377 389 383 383 379 389
                   383 385 381 375 375 381 393 387 379 387 393 393
                   389 387 385 387 381 387 381 385 393 381 385 381
                   383 383 387 391 389 393 383 385 397 389 381 393
                   389 387 385 387 391 387 383 381 387 401 387 381
                   387 381 385 387 379 379 383 379]))
      (test/is (= (mapv taiga/count-leaves (taiga/terms model))
                  [196 195 193 197 191 194 196 193 190 191 190 198
                   191 192 197 196 197 193 194 195 193 188 194 199
                   189 199 194 189 199 196 188 192 194 195 195 194
                   194 197 191 193 195 188 193 191 192 184 193 197
                   190 196 193 196 193 190 201 192 196 190 197 192
                   188 188 194 193 194 186 189 195 192 192 190 195
                   192 193 191 188 188 191 197 194 190 194 197 197
                   195 194 193 194 191 194 191 193 197 191 193 191
                   192 192 194 196 195 197 192 193 199 195 191 197
                   195 194 193 194 196 194 192 191 194 201 194 191
                   194 191 193 194 190 190 192 190]))
      (test/is 
        (= [0.0013131891337110524 1.1205891823684089 0.844569156866623] 
           train-summary))
      (test/is 
        (= [0.013126110562095136 1.1664294816861438 0.8819659085042734]
           test-summary)))))
;;----------------------------------------------------------------