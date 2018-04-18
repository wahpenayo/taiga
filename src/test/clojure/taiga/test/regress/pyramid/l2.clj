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
                  [12 11 14 16 12 11 13 14 14 15 11 15 16 11 14 15 15 16 14 13 12 16 16 17 13 12 11 15 14 11 19 13 15 15 13 12 13 14 17 15 12 13 13 14 13 15 15 15 13 13 15 13 15 15 16 17 12 12 15 12 16 13 12 16 12 12 19 14 12 14 14 14 11 11 16 14 11 14 11 13 16 13 13 14 14 15 12 13 13 17 22 12 13 17 10 12 12 12 14 12 12 13 14 12 14 14 14 13 16 14 12 14 11 13 14 11 13 21 13 12 14 14 12 13 12 13 14 13]))
      (test/is (= (mapv taiga/count-children (taiga/terms model))
                  [385 383 387 383 395 389 389 379 389 387 391 379 379 375 389 391 387 375 377 387 385 379 381 393 387 383 381 379 389 373 387 389 381 383 383 389 391 385 385 375 375 383 387 379 403 383 403 391 391 393 381 379 385 383 383 373 395 397 387 383 375 379 389 375 381 391 387 381 393 395 377 383 389 387 385 385 379 383 385 393 379 377 369 391 383 379 373 377 397 371 389 387 387 381 383 379 393 391 375 377 379 381 383 383 393 387 379 385 373 387 383 385 383 377 389 385 397 389 379 399 385 373 383 381 387 393 387 383]))
      (test/is (= (mapv taiga/count-leaves (taiga/terms model))
                   [193 192 194 192 198 195 195 190 195 194 196 190 190 188 195 196 194 188 189 194 193 190 191 197 194 192 191 190 195 187 194 195 191 192 192 195 196 193 193 188 188 192 194 190 202 192 202 196 196 197 191 190 193 192 192 187 198 199 194 192 188 190 195 188 191 196 194 191 197 198 189 192 195 194 193 193 190 192 193 197 190 189 185 196 192 190 187 189 199 186 195 194 194 191 192 190 197 196 188 189 190 191 192 192 197 194 190 193 187 194 192 193 192 189 195 193 199 195 190 200 193 187 192 191 194 197 194 192]))
      (test/is 
        (= {:rmean 7.484777936196639E-5, :rmse 0.9727892250448811, :rmad 0.6746817223390487, :rmqr 0.6746817223390487, :rmrq 0.6746817223390487} 
           train-summary))
      (test/is 
        (= {:rmean 0.0036339609344836184, :rmse 1.014054848227105, :rmad 0.7052044018971307, :rmqr 0.7052044018971307, :rmrq 0.7052044018971307}
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
        (=  {:rmean 0.0013131891337110524, 
             :rmse 1.1205891823684089,
             :rmad 0.844569156866623, 
             :rmqr 0.844569156866623,
             :rmrq 0.844569156866623} 
           train-summary))
      (test/is 
        (= {:rmean 0.013126110562095136, 
            :rmse 1.1664294816861438, 
            :rmad 0.8819659085042734, 
            :rmqr 0.8819659085042734, 
            :rmrq 0.8819659085042734}
           test-summary)))))
;;----------------------------------------------------------------