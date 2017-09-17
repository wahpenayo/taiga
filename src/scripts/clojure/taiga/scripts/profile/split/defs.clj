(set! *warn-on-reflection* true)
(set! *unchecked-math* false)
(ns ^{:author "John Alan McDonald" :date "2016-11-23"
      :doc "scored vs all subsets." }
    
    taiga.scripts.profile.split.defs
  
  (:require [criterium.core :as criterium]
            [zana.api :as z]
            [taiga.forest :as forest]
            [taiga.scripts.data.defs :as defs]
            [taiga.scripts.data.record :as record]))
(set! *unchecked-math* :warn-on-boxed)
;;------------------------------------------------------------------------------
(defn primate-options []
  (z/reset-mersenne-twister-seeds)
  (let [options (defs/options (record/make-pyramid-function 1.0) (* 64 1024))
        options (forest/positive-fraction-probability-options options)
        n (count (:data options))
        ;; cache arrays, mutated during numerical split optimization
        x0 (double-array n)
        y0 (double-array n)
        w0 (when (:weight options) (double-array n))
        y1 (double-array n)
        w1 (when (:weight options) (double-array n))
        perm (when (:weight options) (int-array n))]
    (assoc options 
           :ground-truth record/true-class
           :x0 x0 :y0 y0 :w0 w0 :perm perm :y1 y1 :w1 w1
           :this-predictor [:primate record/primate])))
;;------------------------------------------------------------------------------
(defn kolor-options []
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
        perm (when (:weight options) (int-array n))]
    (assoc options 
           :ground-truth record/true-class
           :x0 x0 :y0 y0 :w0 w0 :perm perm :y1 y1 :w1 w1
           :this-predictor [:kolor record/kolor])))
;;------------------------------------------------------------------------------
(defn bench [splitter options]  
  (println (z/name (first (:this-predictor options)))) 
  (criterium/bench (splitter options)))
;;------------------------------------------------------------------------------
