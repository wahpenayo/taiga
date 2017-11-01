(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-12-22"
      :doc "Pyramid function probability forest example." }
    
    taiga.test.classify.pyramid.bias-weight-majority-vote-probability
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.classify.data.record :as record]
            [taiga.test.classify.data.defs :as defs]))
;; mvn -Dtest=taiga.test.classify.pyramid.bias-weight-majority-vote-probability clojure:test > tests.txt
;;------------------------------------------------------------------------------
(def nss (str *ns*))
(test/deftest pyramid-bias-weight-majority-vote-probability
  (z/seconds 
    nss
    (z/reset-mersenne-twister-seeds)
    (let [bias 0.1
          options (defs/options (record/make-pyramid-function bias))
          options (assoc options :weight (defs/weight bias))
          forest (taiga/majority-vote-probability options)
          _ (z/mapc #(tree/check-mincount options %) (taiga/terms forest))
          ;;_ (defs/serialization-test nss options forest)
          predictors (into (sorted-map)
                           (dissoc record/attributes :ground-truth :prediction))
          ^clojure.lang.IFn$OD p record/true-probability
          ^clojure.lang.IFn$OD phat (fn phat ^double [datum] 
                                      (* (/ bias (- 1.0 bias)) 
                                         (.invokePrim forest predictors datum)))
          ^clojure.lang.IFn$OD chat (fn chat ^double [datum] 
                                      (if (< (.invokePrim phat datum) 0.5) 0.0 1.0))
          train-confusion (defs/confusion record/true-class chat (:data options))
          test-confusion (defs/confusion record/true-class chat (:test-data options))
          train-mad (z/mean-absolute-difference p phat (:data options))
          test-mad (z/mean-absolute-difference p phat (:test-data options))]
      (test/is (= (mapv taiga/node-height (taiga/terms forest))
                  [19 19 18 21 16 20 18 24 17 24 20 17 21 17 21 17 19 17 18 18 15 20 20 21 19 18 16 18 24 19 25 18 17 17 19 17 21 19 25 15 18 17 19 22 21 18 26 17 24 20 15 17 18 20 19 20 19 22 18 20 21 27 15 18 24 19 20 20 24 21 18 17 19 21 21 26 24 21 23 21 16 18 18 19 21 19 21 19 21 23 17 22 19 20 16 19 20 19 18 18 20 16 19 24 19 25 16 22 21 18 19 19 21 17 17 24 20 16 17 18 23 18 23 23 24 16 25 20]  ))
      (test/is (= (mapv taiga/count-children (taiga/terms forest))
                  [361 363 369 363 377 361 347 363 369 365 361 379 359 357 359 367 385 389 357 361 363 367 375 365 357 367 357 377 363 363 379 375 369 359 369 371 371 377 349 377 365 375 371 375 373 373 371 365 351 363 365 363 373 363 375 371 373 363 363 355 375 361 373 375 367 359 375 353 371 367 371 359 357 365 375 361 357 363 367 367 363 365 363 361 363 361 371 353 365 357 389 367 355 375 357 367 379 373 375 353 361 365 377 361 377 365 359 367 369 365 373 373 367 363 373 361 381 353 375 363 357 369 365 363 367 363 373 371]   ))
      (test/is (= (mapv taiga/count-leaves (taiga/terms forest))
                  [181 182 185 182 189 181 174 182 185 183 181 190 180 179 180 184 193 195 179 181 182 184 188 183 179 184 179 189 182 182 190 188 185 180 185 186 186 189 175 189 183 188 186 188 187 187 186 183 176 182 183 182 187 182 188 186 187 182 182 178 188 181 187 188 184 180 188 177 186 184 186 180 179 183 188 181 179 182 184 184 182 183 182 181 182 181 186 177 183 179 195 184 178 188 179 184 190 187 188 177 181 183 189 181 189 183 180 184 185 183 187 187 184 182 187 181 191 177 188 182 179 185 183 182 184 182 187 186]   ))
      (println "train:") (println train-confusion)
      (println "test:") (println test-confusion)
      (test/is (== (z/count (:data options)) (reduce + train-confusion)))
      (test/is (= [31102 1666 0 0] train-confusion))
      (test/is (== (z/count (:test-data options)) (reduce + test-confusion)))
      (test/is (= [31071 1697 0 0] test-confusion))
      (println "train" train-mad)
      (test/is (== (float train-mad) (float 0.030535277070127105)))
      (println "test" test-mad)
      (test/is (== (float test-mad) (float 0.03070983204532861)))
      (defs/write-predictions 
        nss options "train" forest
        (defs/predictions phat (:data options) :predicted-probability))
      (defs/write-predictions 
        nss options "test"  forest
        (defs/predictions phat (:test-data options) :predicted-probability))
      #_(defs/by-nterms nss options forest predictors))))
;;------------------------------------------------------------------------------
