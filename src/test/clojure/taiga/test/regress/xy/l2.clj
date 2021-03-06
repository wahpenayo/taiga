(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-17"
      :doc "1d y = a*x + b data and l2 regression models." }
    
    taiga.test.regress.xy.l2
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.xy :as xy]
            [taiga.test.regress.data.defs :as defs]))
;; mvn -Dtest=taiga.test.regress.xy.l2 clojure:test > xy-l2.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
;;----------------------------------------------------------------
(test/deftest noiseless-affine
  (z/reset-mersenne-twister-seeds)
  (z/seconds 
    nss
    (let [options (defs/options 
                    xy/attributes
                    xy/xbindings
                    xy/generator
                    (xy/make-xy-function 1.0 2.0)
                    -1.0)
          model (taiga/affine-l2-regression options)
          _ (defs/edn-test model (defs/affine-edn-file nss))
          y (:ground-truth xy/attributes)
          yhat (fn yhat ^double [datum] 
                 (model xy/xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          _ (println "test:" )
          test-summary (defs/print-residual-summary 
                         y yhat (:test-data options))]
      (test/is (=  {:rmean -1.7604732775733378E-17, 
                    :rmse 8.841986127560332E-17, 
                    :rmad 1.7604732775733378E-17, 
                    :rmqr 1.7604732775733378E-17, 
                    :rmrq 1.7604732775733378E-17}
                   train-summary))
      (test/is (= {:rmean -1.8241701552068612E-17,
                   :rmse 9.000523778413571E-17, 
                   :rmad 1.8241701552068612E-17, 
                   :rmqr 1.8241701552068612E-17, 
                   :rmrq 1.8241701552068612E-17}
                  test-summary)))))
;;----------------------------------------------------------------
(test/deftest affine
  (z/reset-mersenne-twister-seeds)
  (z/seconds 
    nss
    (let [options (defs/options 
                    xy/attributes
                    xy/xbindings
                    xy/generator
                    (xy/make-xy-function 1.0 2.0)
                    0.5)
          model (taiga/affine-l2-regression options)
          _ (defs/edn-test model (defs/affine-edn-file nss))
          y (:ground-truth xy/attributes)
          yhat (fn yhat ^double [datum] 
                 (model xy/xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          _ (println "test:" )
          test-summary (defs/print-residual-summary 
                         y yhat (:test-data options))]
      (test/is 
        (z/maps-approximately==
          1.0e0
          {:rmean -5.425130830682967E-15, 
           :rmse 0.5004578997453809, 
           :rmad 0.39929176914302816, 
           :rmqr 0.39929176914302816, 
           :rmrq 0.39929176914302816}
          train-summary))
      (test/is  
        (z/maps-approximately==
          1.0e0
          {:rmean -0.004412699741725591, 
           :rmse 0.5005329599483825, 
           :rmad 0.40041545472567863, 
           :rmqr 0.40041545472567863, 
           :rmrq 0.40041545472567863}
          test-summary)))))
;;----------------------------------------------------------------
(test/deftest forest
  (z/reset-mersenne-twister-seeds)
  (z/seconds 
    nss
    (let [options (defs/options 
                    xy/attributes
                    xy/xbindings
                    xy/generator
                    (xy/make-xy-function 2.0 1.0)
                    0.5)
          model (taiga/mean-regression options)
          _ (z/mapc #(tree/check-mincount options %) 
                    (taiga/terms model))
          _ (defs/json-test nss options model)
          _ (defs/edn-test 
              model (defs/forest-file nss options model))
          y (:ground-truth xy/attributes)
          yhat (fn yhat ^double [datum] 
                 (model xy/xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          _ (println "test:" )
          test-summary (defs/print-residual-summary 
                         y yhat (:test-data options))]
      (test/is (= (mapv taiga/node-height (taiga/terms model))
                  [9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
                   9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
                   9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
                   9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
                   9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
                   9 9 9 9 9 9 9 9]))
      (test/is (= (mapv taiga/count-children (taiga/terms model))
                  [377 393 389 395 393 389 395 395 393 399 391 383
                   387 387 379 375 377 379 389 383 385 375 387 391
                   395 391 395 385 387 389 387 391 403 391 377 399
                   401 391 375 399 379 387 381 385 397 389 385 395
                   401 395 395 405 387 397 395 385 391 385 391 387
                   383 389 389 385 373 393 405 393 383 401 385 391
                   389 375 385 395 401 375 399 385 391 391 403 385
                   405 385 387 387 395 389 389 395 395 393 383 385
                   387 391 383 387 387 393 389 373 389 381 397 379
                   391 385 389 399 393 391 383 375 393 385 387 383
                   391 389 389 389 383 395 383 389]))
      (test/is (= (mapv taiga/count-leaves (taiga/terms model))
                  [189 197 195 198 197 195 198 198 197 200 196 192
                   194 194 190 188 189 190 195 192 193 188 194 196
                   198 196 198 193 194 195 194 196 202 196 189 200
                   201 196 188 200 190 194 191 193 199 195 193 198
                   201 198 198 203 194 199 198 193 196 193 196 194
                   192 195 195 193 187 197 203 197 192 201 193 196
                   195 188 193 198 201 188 200 193 196 196 202 193
                   203 193 194 194 198 195 195 198 198 197 192 193
                   194 196 192 194 194 197 195 187 195 191 199 190
                   196 193 195 200 197 196 192 188 197 193 194 192
                   196 195 195 195 192 198 192 195]))
      (test/is (= {:rmean -0.004200668656316388,
                   :rmse 0.5168940586637416, 
                   :rmad 0.41132787537301707, 
                   :rmqr 0.41132787537301707, 
                   :rmrq 0.41132787537301707} 
                  train-summary))
      (test/is (= {:rmean -0.01950607171796734, 
                   :rmse 0.5546694731654364,
                   :rmad 0.4427264242525454,
                   :rmqr 0.4427264242525454,
                   :rmrq 0.4427264242525454}
                  test-summary)))))
;;----------------------------------------------------------------
(test/deftest noiseless-forest
  (z/reset-mersenne-twister-seeds)
  (z/seconds 
    nss
    (let [options (defs/options 
                    xy/attributes
                    xy/xbindings
                    xy/generator
                    (xy/make-xy-function 2.0 1.0)
                    -1.0)
          model (taiga/mean-regression options)
          _ (z/mapc #(tree/check-mincount options %) 
                    (taiga/terms model))
          _ (defs/json-test nss options model)
          _ (defs/edn-test 
              model (defs/forest-file nss options model))
          y (:ground-truth xy/attributes)
          yhat (fn yhat ^double [datum] 
                 (model xy/xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          _ (println "test:" )
          test-summary (defs/print-residual-summary 
                         y yhat (:test-data options))]
      (test/is (= (mapv taiga/node-height (taiga/terms model))
                  [9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
                   9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
                   9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
                   9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
                   9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
                   9 9 9 9 9 9 9 9]))
      (test/is (= (mapv taiga/count-children (taiga/terms model))
                  [377 377 401 391 397 389 395 383 369 393 399 389
                   393 387 387 391 393 397 391 389 383 403 397 395
                   381 385 389 391 391 395 389 387 389 403 391 387
                   387 393 381 395 387 385 391 379 383 385 393 387
                   397 383 389 395 389 395 393 385 403 385 389 391
                   387 381 395 375 387 393 401 391 387 401 393 397
                   385 401 387 389 393 387 385 385 389 403 395 391
                   397 393 381 383 399 379 395 383 381 397 389 387
                   399 383 389 393 387 387 385 385 405 391 391 387
                   393 391 385 405 397 393 387 395 389 377 375 391
                   395 395 391 393 391 391 397 391]))
      (test/is (= (mapv taiga/count-leaves (taiga/terms model))
                  [189 189 201 196 199 195 198 192 185 197 200 195
                   197 194 194 196 197 199 196 195 192 202 199 198
                   191 193 195 196 196 198 195 194 195 202 196 194
                   194 197 191 198 194 193 196 190 192 193 197 194
                   199 192 195 198 195 198 197 193 202 193 195 196
                   194 191 198 188 194 197 201 196 194 201 197 199
                   193 201 194 195 197 194 193 193 195 202 198 196
                   199 197 191 192 200 190 198 192 191 199 195 194
                   200 192 195 197 194 194 193 193 203 196 196 194
                   197 196 193 203 199 197 194 198 195 189 188 196
                   198 198 196 197 196 196 199 196]))
      (test/is (= {:rmean -0.009317156875444071, 
                   :rmse 0.2280583200826239, 
                   :rmad 0.17506472425026406, 
                   :rmqr 0.17506472425026406,
                   :rmrq 0.17506472425026406}
                  train-summary))
      (test/is (= {:rmean -0.026503320776710828, 
                   :rmse 0.2376067380534039, 
                   :rmad 0.18277868732700348,
                   :rmqr 0.18277868732700348, 
                   :rmrq 0.18277868732700348}
                  test-summary)))))
;;----------------------------------------------------------------
