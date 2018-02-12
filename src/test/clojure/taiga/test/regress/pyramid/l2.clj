(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-02-10"
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
                  [18 17 19 17 20 19 16 20 18 21 19 23 19 21 21 17
                   20 19 17 17 19 21 17 22 18 18 17 18 21 18 20 16
                   21 19 18 19 16 19 22 17 22 21 17 23 18 20 17 19
                   21 19 18 17 21 26 22 18 17 23 18 16 22 16 17 20
                   17 17 19 20 16 17 17 16 16 19 20 18 18 19 17 17
                   21 17 17 24 21 18 20 21 18 21 23 18 17 17 18 15
                   17 16 17 18 18 17 18 18 17 18 20 17 19 17 17 18
                   20 19 17 16 19 22 21 19 17 18 18 18 19 18 18 17]))
      (test/is (= (mapv taiga/count-children (taiga/terms model))
                  [395 389 403 403 391 397 391 387 395 401 407 393
                   383 395 399 401 403 405 405 399 393 393 395 387
                   407 401 391 401 403 401 401 411 395 393 389 401
                   383 391 399 393 399 401 397 401 413 405 395 403
                   409 407 393 391 391 401 397 393 397 397 403 393
                   387 401 399 403 393 381 387 405 393 403 403 387
                   397 397 395 395 399 405 391 393 393 395 397 405
                   391 403 393 401 395 407 397 407 403 403 399 407
                   405 395 401 401 389 381 397 395 385 399 397 399
                   395 395 401 403 397 399 399 391 409 403 405 401
                   393 397 397 397 401 395 395 401]  ))
      (println (mapv taiga/count-leaves (taiga/terms model)))
      (test/is (= (mapv taiga/count-leaves (taiga/terms model))
                  [198 195 202 202 196 199 196 194 198 201 204 197
                   192 198 200 201 202 203 203 200 197 197 198 194
                   204 201 196 201 202 201 201 206 198 197 195 201
                   192 196 200 197 200 201 199 201 207 203 198 202
                   205 204 197 196 196 201 199 197 199 199 202 197
                   194 201 200 202 197 191 194 203 197 202 202 194
                   199 199 198 198 200 203 196 197 197 198 199 203
                   196 202 197 201 198 204 199 204 202 202 200 204
                   203 198 201 201 195 191 199 198 193 200 199 200
                   198 198 201 202 199 200 200 196 205 202 203 201
                   197 199 199 199 201 198 198 201]  ))
      (test/is 
        (= [-0.002335092656504358
            1.496475255561478
            1.1660535004533745] 
           train-summary))
      (test/is 
        (= [-0.018364270422739106 
            1.546529953331924 
            1.2067770415287329]
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
                  [18 17 18 18 20 19 18 19 16 18 21 21 21 18 20 18 
                   19 19 17 19 18 21 19 20 17 18 17 17 21 18 19 17 
                   19 19 17 17 16 22 21 16 18 22 16 21 17 18 18 18 
                   16 18 19 18 18 22 18 20 17 22 21 16 22 16 18 20 
                   17 19 16 21 19 17 16 15 19 18 20 20 18 18 19 19 
                   19 18 20 24 17 19 16 16 16 21 20 17 21 20 16 16 
                   16 18 17 16 19 17 19 16 18 19 18 17 20 17 18 19 
                   18 16 17 18 17 20 20 19 16 17 16 16 17 18 16 16]))
      (test/is (= (mapv taiga/count-children (taiga/terms model))
                  [405 401 393 403 401 399 403 397 397 403 397 397 
                   399 395 393 395 401 401 401 387 397 387 387 389 
                   403 403 389 399 385 403 395 405 401 391 403 395 
                   399 401 395 399 399 397 399 403 397 397 399 389 
                   399 395 407 393 397 401 399 393 399 395 405 393 
                   409 389 393 399 387 393 407 401 403 387 403 389 
                   399 391 385 405 399 401 385 397 393 391 397 411 
                   389 393 401 407 397 407 395 409 405 405 383 407 
                   395 395 389 401 391 407 403 395 413 401 399 409 
                   399 403 393 403 395 399 397 385 399 391 403 399 
                   393 401 397 389 401 397 389 397]))
      (test/is (= (mapv taiga/count-leaves (taiga/terms model))
                  [203 201 197 202 201 200 202 199 199 202 199 199
                   200 198 197 198 201 201 201 194 199 194 194 195
                   202 202 195 200 193 202 198 203 201 196 202 198
                   200 201 198 200 200 199 200 202 199 199 200 195
                   200 198 204 197 199 201 200 197 200 198 203 197
                   205 195 197 200 194 197 204 201 202 194 202 195
                   200 196 193 203 200 201 193 199 197 196 199 206
                   195 197 201 204 199 204 198 205 203 203 192 204
                   198 198 195 201 196 204 202 198 207 201 200 205
                   200 202 197 202 198 200 199 193 200 196 202 200
                   197 201 199 195 201 199 195 199]))
      (test/is 
        (= [-0.004207704629091607 
            1.5461839228589027 
            1.2122981269210311] 
           train-summary))
      (test/is 
        (= [-0.012399970117878413 
            1.6023989634694786 
            1.2540134251513577]
           test-summary)))))
;;----------------------------------------------------------------