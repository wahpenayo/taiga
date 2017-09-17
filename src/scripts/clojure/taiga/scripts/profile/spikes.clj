(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-07-06"
      :doc "Profile spikes function probability forest example." }
    
    taiga.scripts.profile.spikes
  
  (:require [clojure.test :as test]
            [taiga.test.spikes.majority-vote-probability]
            [taiga.test.spikes.positive-fraction-probability]))
;;------------------------------------------------------------------------------
(test/run-tests 'taiga.test.spikes.majority-vote-probability)
(test/run-tests 'taiga.test.spikes.positive-fraction-probability)
;;------------------------------------------------------------------------------
