(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-07-08"
      :doc "Profile pyramid function probability forest example." }
    
    taiga.scripts.profile.pyramid
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.test.pyramid.bias-majority-vote-probability]
            [taiga.test.pyramid.bias-positive-fraction-probability]
            [taiga.test.pyramid.bias-weight-majority-vote-probability]
            [taiga.test.pyramid.bias-weight-positive-fraction-probability]
            [taiga.test.pyramid.classifier]
            [taiga.test.pyramid.majority-vote-probability]
            [taiga.test.pyramid.positive-fraction-probability]))
;;------------------------------------------------------------------------------
(z/seconds
  (print-str *ns*)
  (test/run-tests 'taiga.test.pyramid.bias-majority-vote-probability)
  (test/run-tests 'taiga.test.pyramid.bias-positive-fraction-probability)
  (test/run-tests 'taiga.test.pyramid.bias-weight-majority-vote-probability)
  (test/run-tests 'taiga.test.pyramid.bias-weight-positive-fraction-probability)
  (test/run-tests 'taiga.test.pyramid.classifier)
  (test/run-tests 'taiga.test.pyramid.majority-vote-probability)
  (test/run-tests 'taiga.test.pyramid.positive-fraction-probability))
;;------------------------------------------------------------------------------
