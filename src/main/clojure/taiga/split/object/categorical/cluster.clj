(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2017-01-03"
      :doc "Used in bottom up category cluster for heuristic split generation." }
    
    taiga.split.object.categorical.cluster
  
  (:refer-clojure :exclude [merge])
  (:require [clojure.set :as set]
            [zana.api :as z]))
;;------------------------------------------------------------------------------
(deftype Cluster [^java.util.Set categories
                  ^java.util.Collection data
                  ^Object left
                  ^Object right
                  ^double cost
                  ^boolean feasible-leaf?])
;;------------------------------------------------------------------------------
(defn categories ^java.util.Collection [^Cluster c] (.categories c))
(defn data ^java.util.Collection [^Cluster c] (.data c))
(defn left ^taiga.split.object.categorical.cluster.Cluster [^Cluster c] (.left c))
(defn right ^taiga.split.object.categorical.cluster.Cluster [^Cluster c] (.right c))
(defn cost ^double [^Cluster c] (.cost c))
(defn cost ^double [^Cluster c] (.cost c))
(defn feasible-leaf? ^Boolean [^Cluster this] 
  (Boolean/valueOf (.feasible-leaf? this)))
;;------------------------------------------------------------------------------
(defn merge-cost ^double [^Cluster this] 
  (assert (and (left this) (right this)) "No children, no merge cost!")
  (- (cost this) (cost (left this)) (cost (right this))))
;;------------------------------------------------------------------------------
(defn forget-children ^taiga.split.object.categorical.cluster.Cluster [^Cluster this] 
  (Cluster. 
    (categories this) (data this) nil nil (cost this) (feasible-leaf? this)))
;;------------------------------------------------------------------------------
(defn make 
  ^taiga.split.object.categorical.cluster.Cluster [^java.util.Set categories
                                                   ^java.util.List data
                                                   ^Cluster left
                                                   ^Cluster right
                                                   ^clojure.lang.IFn y
                                                   ^clojure.lang.IFn cost-factory
                                                   ^clojure.lang.IFn feasible?]
  (let [^zana.java.accumulator.Accumulator cost (cost-factory)
        n (.size data)]
    (dotimes [i n] (.add cost (y (.get data i))))
    (Cluster. categories data left right (.doubleValue cost) (feasible? cost))))
;;------------------------------------------------------------------------------
(defn merge 
  ^taiga.split.object.categorical.cluster.Cluster [^Cluster left 
                                                   ^Cluster right
                                                   ^clojure.lang.IFn y
                                                   ^clojure.lang.IFn cost-factory
                                                   ^clojure.lang.IFn feasible?] 
  #_(assert (empty? (set/intersection (categories left) (categories right))))
  (let [cats (z/concat (categories left) (categories right))
        data 
        #_(com.google.common.collect.Iterables/concat (data left) (data right))
        (z/concat (data left) (data right))]
    (make cats data left right y cost-factory feasible?)))
;;------------------------------------------------------------------------------
