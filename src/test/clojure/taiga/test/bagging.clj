(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-11-09"
      :doc "Bootstrap aggregation (bagging) checks." }
    
    taiga.test.bagging
  
  (:require [clojure.pprint :as pp]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.classify.data.record :as record]
            [taiga.test.classify.data.defs :as defs]))
;; mvn -Dmaven.test.skip=true clean install
;; mvn -Dtest=taiga.test.bagging clojure:test
;;------------------------------------------------------------------------------
;; TODO: make this a function so we can test with every data set.
;;------------------------------------------------------------------------------
(defn- freqs ^double [label data]
  (let [data-freqs (z/frequencies record/true-class data)
        frac (/ (double (z/get data-freqs 1.0)) (z/count data))]
    #_(println label (z/count data) "\n" frac)
    frac)) 
;;------------------------------------------------------------------------------
(test/deftest bagging
  (z/reset-mersenne-twister-seeds)
  (let [options (defs/options (record/make-spikes-function 1.0) (* 1024 1024))
        prng (z/mersenne-twister-generator)
        data (:data options)
        fd (freqs "data" data)]
    (dotimes [i 8]
      (let [bag (z/sample-with-replacement prng data)
            data-bag (z/intersection data bag)
            fb (freqs "bag" bag)
            fi (freqs "intersection" data-bag)
            coverage (float (/ (z/count data-bag) (z/count data)))]
        (test/is (== (z/count bag) (z/count data)))
        (test/is (< 0.631 coverage 0.633))
        (test/is (< 0.988 (/ fb fd) 1.012))
        (test/is (< 0.988 (/ fi fd) 1.012))))))
;;------------------------------------------------------------------------------
