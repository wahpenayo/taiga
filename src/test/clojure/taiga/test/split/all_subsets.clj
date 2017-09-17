(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-11-23"
      :doc "scored vs all subsets." }
    
    taiga.test.split.all-subsets
  
  (:require [clojure.pprint :as pp]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.split.api :as split]
            [taiga.split.numerical.categorical.scored :as scored]
            [taiga.split.object.categorical.all-subsets :as all-subsets]
            [taiga.split.object.categorical.bottom-up :as bottom-up]
            [taiga.split.object.categorical.heuristic :as heuristic]
            [taiga.forest :as forest]
            [taiga.test.data.record :as record]
            [taiga.test.data.defs :as defs]))
;; mvn -Dtest=taiga.test.split.all-subsets clojure:test > tests.txt
;;------------------------------------------------------------------------------
(test/deftest primate
  (z/reset-mersenne-twister-seeds)
  (let [options (defs/options (record/make-pyramid-function 1.0))
        options (forest/positive-fraction-probability-options options)
        n (count (:data options))
          ;; cache arrays, mutated during numerical split optimization
          x0 (double-array n)
          y0 (double-array n)
          w0 (when (:weight options) (double-array n))
          y1 (double-array n)
          w1 (when (:weight options) (double-array n))
          perm (when (:weight options) (int-array n))
          options (assoc options 
                         :ground-truth record/true-class
                         :x0 x0 :y0 y0 :w0 w0 :perm perm :y1 y1 :w1 w1
                         :this-predictor [:primate record/primate])
        scored (z/seconds "scored" (scored/split options))
        all-subsets (z/seconds "all-subsets" (all-subsets/split options))
        bottom-up (z/seconds "bottom-up" (bottom-up/split options))
        heuristic (z/seconds "heuristic" (heuristic/split options))]
    (println scored)
    (println all-subsets)
    (println bottom-up)
    (println heuristic)
    ;; bottom-up won't always be the same, but scored and all-subsets should be
    (test/is (= scored all-subsets bottom-up heuristic))))
;;------------------------------------------------------------------------------
(test/deftest olor
  (z/reset-mersenne-twister-seeds)
  (let [options (defs/options (record/make-pyramid-function 1.0))
        options (forest/positive-fraction-probability-options options)
        n (count (:data options))
          ;; cache arrays, mutated during numerical split optimization
          x0 (double-array n)
          y0 (double-array n)
          w0 (when (:weight options) (double-array n))
          y1 (double-array n)
          w1 (when (:weight options) (double-array n))
          perm (when (:weight options) (int-array n))
          options (assoc options 
                         :ground-truth record/true-class
                         :x0 x0 :y0 y0 :w0 w0 :perm perm :y1 y1 :w1 w1
                         :this-predictor [:primate record/kolor])
        scored (z/seconds "scored" (scored/split options))
        all-subsets (z/seconds "all-subsets" (all-subsets/split options))
        bottom-up (z/seconds "bottom-up" (bottom-up/split options))
        heuristic (z/seconds "heuristic" (heuristic/split options))]
    (println scored)
    (println all-subsets)
    (println bottom-up)
    (println heuristic)
    ;; bottom-up won't always be the same, but scored and all-subsets should be
    (test/is (= scored all-subsets bottom-up heuristic))))
;;------------------------------------------------------------------------------
