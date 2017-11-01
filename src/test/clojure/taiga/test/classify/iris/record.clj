(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "Kristina Lisa Klinkner, John Alan McDonald)" :date "2015-12-07"
      :doc "Iris data for random forest classification example." }

    taiga.test.classify.iris.record
  
  (:require [zana.api :as z]))
;;------------------------------------------------------------------------------
(z/define-datum Record
  [^double [sepal-length (fn [t _] (Double/parseDouble (:sepallength t)))]
   ^double [sepal-width (fn [t _] (Double/parseDouble (:sepalwidth t)))]
   ^double [petal-length (fn [t _] (Double/parseDouble (:petallength t)))]
   ^double [petal-width  (fn [t _] (Double/parseDouble (:petalwidth t)))]
   ^double species])

(def attributes
  (merge {:ground-truth species}
         (into {} (map #(vector (keyword (z/name %)) %)
                       [sepal-length sepal-width petal-length petal-width]))))
;;------------------------------------------------------------------------------
