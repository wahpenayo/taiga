(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2016-12-19"
      :doc "Heuristic search based on number of categories." }
    
    taiga.split.object.categorical.heuristic
  
  (:require [zana.api :as z]
            [taiga.split.object.categorical.all-subsets :as all-subsets]
            [taiga.split.object.categorical.bottom-up :as bottom-up]))
;;------------------------------------------------------------------------------
;; TODO: determine cutoff for all subsets search using performance scripts
(def ^Long/TYPE cutoff 
  "How many categories is too many for an all subsets search?"
  11)
;;------------------------------------------------------------------------------
(defn split [options]
  (let [[_ ^clojure.lang.IFn x] (:this-predictor options)
        n (z/count-distinct-not-missing x (:data options))]
    (if (< n cutoff)
      (all-subsets/split options)
      (bottom-up/split options))))
;;------------------------------------------------------------------------------
