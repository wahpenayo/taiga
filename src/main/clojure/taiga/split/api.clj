(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2016-12-30"
      :doc "Greedy decision tree splitting." }
    
    taiga.split.api
  
  (:require [zana.api :as z]
            [taiga.split.numerical.categorical.scored :as numerical-categorical]
            [taiga.split.numerical.categorical.weighted-scored :as weighted-categorical]
            [taiga.split.numerical.eenum.simple :as numerical-enum]
            [taiga.split.numerical.eenum.weighted :as weighted-enum]
            [taiga.split.numerical.numerical.xy :as numerical-numerical]
            [taiga.split.numerical.numerical.xyw :as numerical-weighted]
            [taiga.split.object.categorical.heuristic :as object-categorical]
            [taiga.split.object.numerical.xy :as object-numerical])
  (:import [java.util List]
           [clojure.lang IFn IFn$OD]
           [zana.java.accumulator Accumulator]))
;;------------------------------------------------------------------------------
;; TODO: dis-entangle mincount test from cost functions, or at least make it
;; easier to just check data.

(defn mincount-split? 
  
  ([^long mincount 
    ^zana.java.accumulator.Accumulator cost]
    
    (<= mincount (.netCount cost)))
  
  ([^long mincount 
    ^zana.java.accumulator.Accumulator cost0 
    ^zana.java.accumulator.Accumulator cost1]
    
    (and (mincount-split? mincount cost0)
         (mincount-split? mincount cost1))))
;;------------------------------------------------------------------------------
(defn- check-best-split-options [options]
  (assert (not (empty? (:predictors options)))
          (print-str "No :predictors in:" (z/pprint-map-str options)))
  (let [^Iterable data (:data options)
        ^IFn y (:ground-truth options)
        ^IFn$OD w (:weight options)
        cost-factory (:cost-factory options)]
    (assert (not (empty? data))
            (print-str "No :data\n" (z/pprint-map-str options)))
    (assert (instance? IFn y) 
            (print-str "No :ground-truth\n" (z/pprint-map-str options)))
    (assert (or (not w) (instance? IFn$OD w)) 
            (print-str "invalid :weight\n" (z/pprint-map-str options)))
    (assert (ifn? cost-factory))))
;;------------------------------------------------------------------------------
(def enum-valued? (memoize z/enum-valued?))
;;------------------------------------------------------------------------------
(defn- attribute-split [options]
  (let [^IFn y (:ground-truth options)
        [_ x] (:this-predictor options)]
    (assert (or (instance? IFn$OD y) (nil? (:weight options))))
    (if (instance? IFn$OD y)
      
      (cond (enum-valued? x) (if (:weight options)
                               (weighted-enum/split options)
                               (numerical-enum/split options))
            (z/numerical? x) (if (:weight options)
                               (numerical-weighted/split options)
                               (numerical-numerical/split options))
            :else (if (:weight options)
                    (weighted-categorical/split options)
                    (numerical-categorical/split options)))
      
      (if (z/numerical? x) 
        (object-numerical/split options)
        (object-categorical/split options)))))
;;------------------------------------------------------------------------------
(defn- allocate-cache [^IFn y ^IFn$OD w ^List data]
  (let [n (z/count data)]
    (if w
      (do 
        (assert (instance? IFn$OD w))
        (assert (instance? IFn$OD y))
        (numerical-weighted/allocate-cache n))
      (if (instance? IFn$OD y)
        (numerical-numerical/allocate-cache n)
        #_(object-numerical/allocate-cache y data)
        (object-numerical/allocate-cache n)))))
;;------------------------------------------------------------------------------
(defn best-split [options]
  ;; no need to split if y constant
  (when-not (z/singular? (:ground-truth options) (:data options))
    (check-best-split-options options)
    (let [^List data (:data options)
          n (z/count data)
          ^IFn y (:ground-truth options)
          ^IFn$OD w (:weight options)
          ;; cache array, mutated during numerical split optimization
          options (assoc options :xys (allocate-cache y w data))
          xs (:predictors options)]
      (if (> (* 2 (int (:mincount options))) n)
        nil
        (loop [xs xs
               pmin nil
               cmin Double/POSITIVE_INFINITY]
          (if (empty? xs)
            pmin
            (let [x (first xs)
                  s (attribute-split (assoc options :this-predictor x))
                  c (double (:cost s))]
              (if (< c cmin)
                (recur (rest xs) (:split s) c)
                (recur (rest xs) pmin cmin)))))))))
;;------------------------------------------------------------------------------
