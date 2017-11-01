(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-12-22"
      :doc "Pyramid function probability forest example." }
    
    taiga.test.classify.pyramid.bias-majority-vote-probability
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.classify.data.record :as record]
            [taiga.test.classify.data.defs :as defs]))
;; mvn -Dtest=taiga.test.classify.pyramid.bias-majority-vote-probability clojure:test > tests.txt
;;------------------------------------------------------------------------------
(def nss (str *ns*))
(test/deftest pyramid-bias-majority-vote-probability
  (z/seconds 
    nss
    (z/reset-mersenne-twister-seeds)
    (let [options (defs/options (record/make-pyramid-function 0.1))
          forest (taiga/majority-vote-probability options)
          ;;_ (defs/serialization-test nss options forest)
          _ (z/mapc #(tree/check-mincount options %) (taiga/terms forest))
          predictors (into (sorted-map)
                           (dissoc record/attributes :ground-truth :prediction))
          ^clojure.lang.IFn$OD p record/true-probability
          ^clojure.lang.IFn$OD phat (fn phat ^double [datum] 
                                      (.invokePrim forest predictors datum))
          ^clojure.lang.IFn$OD chat (fn chat ^double [datum] 
                                      (if (< (.invokePrim phat datum) 0.5) 0.0 1.0))
          train-confusion (defs/confusion record/true-class chat (:data options))
          test-confusion (defs/confusion record/true-class chat (:test-data options))
          train-mad (z/mean-absolute-difference p phat (:data options))
          test-mad (z/mean-absolute-difference p phat (:test-data options))]
      (test/is (= (mapv taiga/node-height (taiga/terms forest))
                  [19 24 18 20 17 23 18 23 17 18 16 18 20 18 22 19 18 22 21 17 14 19 16 25 20 16 19 21 19 17 23 23 17 21 20 18 22 23 20 20 21 18 20 18 20 21 23 18 19 18 23 22 18 17 19 21 17 19 16 17 23 24 20 21 18 17 23 18 20 21 19 18 18 16 19 22 21 20 19 18 20 19 21 17 17 21 16 23 22 23 17 19 17 27 18 16 19 17 19 19 22 16 18 27 25 20 16 21 21 19 16 18 19 19 22 18 22 17 17 16 17 17 20 21 26 22 20 20]  ))
      (test/is (= (mapv taiga/count-children (taiga/terms forest))
                  [363 377 363 367 373 365 359 363 377 373 371 361 355 359 377 359 373 379 373 365 357 365 361 379 373 369 363 363 371 355 359 369 373 361 367 369 371 361 355 369 361 375 367 375 355 353 369 357 369 359 367 375 355 369 375 373 381 369 365 363 363 371 383 371 361 363 373 379 361 365 369 359 361 361 379 371 373 369 355 365 365 361 353 359 371 359 355 357 357 363 373 365 361 369 365 367 361 371 373 359 367 367 369 365 367 369 361 359 359 365 355 371 357 375 367 371 375 365 361 339 351 375 359 373 363 363 375 375]   ))
      (test/is (= (mapv taiga/count-leaves (taiga/terms forest))
                  [182 189 182 184 187 183 180 182 189 187 186 181 178 180 189 180 187 190 187 183 179 183 181 190 187 185 182 182 186 178 180 185 187 181 184 185 186 181 178 185 181 188 184 188 178 177 185 179 185 180 184 188 178 185 188 187 191 185 183 182 182 186 192 186 181 182 187 190 181 183 185 180 181 181 190 186 187 185 178 183 183 181 177 180 186 180 178 179 179 182 187 183 181 185 183 184 181 186 187 180 184 184 185 183 184 185 181 180 180 183 178 186 179 188 184 186 188 183 181 170 176 188 180 187 182 182 188 188]   ))
      (println "train:") (println train-confusion)
      (println "test:") (println test-confusion)
      (test/is (== (z/count (:data options)) (reduce + train-confusion)))
      (test/is (= [31102 1666 0 0] train-confusion))
      (test/is (== (z/count (:test-data options)) (reduce + test-confusion)))
      (test/is (= [31071 1697 0 0] test-confusion))
      (println "train" train-mad)
      (test/is (== (float train-mad) (float 0.05006848753386445)))
      (println "test" test-mad)
      (test/is (== (float test-mad) (float 0.050462299725640064)))
      (defs/write-predictions 
        nss options "train" forest
        (defs/predictions phat (:data options) :predicted-probability))
      (defs/write-predictions 
        nss options "test"  forest
        (defs/predictions phat (:test-data options) :predicted-probability))
      #_(defs/by-nterms nss options forest predictors))))
;;------------------------------------------------------------------------------
#_(try 
    (test/run-tests)
    (catch Throwable t
      (loop [^Throwable t t]
        (when t
          (.printStackTrace t)
          (recur (.getCause t))))))
;;------------------------------------------------------------------------------
