(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-02-08"
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
          forest (taiga/mean-regression options)
          _ (z/mapc #(tree/check-mincount options %) 
                    (taiga/terms forest))
          _ (defs/serialization-test nss options forest)
          y (:ground-truth record/attributes)
          yhat (fn yhat ^double [datum] 
                 (forest record/xbindings datum))
          train-summary (defs/print-residual-summary
                          y yhat (:data options))
          _ (println "test:" )
          test-summary (defs/print-residual-summary 
                         y yhat (:test-data options))]
      (test/is (= (mapv taiga/node-height (taiga/terms forest))
                  [18 17 18 18 20 19 18 19 16 18 21 21 21 18 20 18 
                   19 19 17 19 18 21 19 20 17 18 17 17 21 18 19 17 
                   19 19 17 17 16 22 21 16 18 22 16 21 17 18 18 18 
                   16 18 19 18 18 22 18 20 17 22 21 16 22 16 18 20 
                   17 19 16 21 19 17 16 15 19 18 20 20 18 18 19 19 
                   19 18 20 24 17 19 16 16 16 21 20 17 21 20 16 16 
                   16 18 17 16 19 17 19 16 18 19 18 17 20 17 18 19 
                   18 16 17 18 17 20 20 19 16 17 16 16 17 18 16 16]))
      (test/is (= (mapv taiga/count-children (taiga/terms forest))
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
                   393 401 397 389 401 397 389 397]  ))
      (test/is (= (mapv taiga/count-leaves (taiga/terms forest))
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
                   197 201 199 195 201 199 195 199]  ))
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
;------------------------------------------------------------------------------