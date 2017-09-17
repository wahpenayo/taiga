(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2016-11-11"
      :doc "Greedy decision tree splitting." }
    
    taiga.utils
  
  (:require [zana.api :as z]))
;;------------------------------------------------------------------------------
(defn sum-alengths ^long [^objects aa]
  (let [n (int (alength aa))]
    (loop [i (int 0)
           sum (int 0)]
      (if (< i n)
        (recur (inc i) (+ sum (alength ^doubles (aget aa i))))
        sum))))
;;------------------------------------------------------------------------------
(defn feasible-cost ^double [^clojure.lang.IFn feasible? 
                             ^zana.java.accumulator.Accumulator cost0 
                             ^zana.java.accumulator.Accumulator cost1]
  (if (feasible? cost0 cost1)
    (+ (double (.doubleValue cost0)) (double (.doubleValue cost1)))
    Double/POSITIVE_INFINITY))
;;------------------------------------------------------------------------------
(defn enum-set 
  
  (^java.util.EnumSet [^java.util.Collection things] 
    (java.util.EnumSet/copyOf things))
  
  (^java.util.EnumSet [^objects xs ^long start ^long end]
    (let [start (int start)
          end (int end)
          n (- end start)]
      (let [eset (java.util.EnumSet/of (aget xs start)) ]
        (loop [i (inc start)]
          (when (< i end)
            (.add eset (aget xs i))
            (recur (inc i))))
        eset))))
;;------------------------------------------------------------------------------
(defn immutable-set ^java.util.Set [^objects xs ^long start ^long end]
  (let [start (int start)
        end (int end)
        n (- end start)]
    (let [builder (com.google.common.collect.ImmutableSet/builder) ]
      (loop [i start]
        (when (< i end)
          (.add builder (aget xs i))
          (recur (inc i))))
      (.build builder))))
;;------------------------------------------------------------------------------
(defn to-set 
  
  (^java.util.Set [^Iterable things]
    (cond 
      (== 1 (z/count things)) (java.util.Collections/singleton (z/first things))
      ;;(same-enums? things) (enum-set things)
      (instance? java.util.Set things) things
      :else (com.google.common.collect.ImmutableSet/copyOf things)))
  
  (^java.util.Set [^objects xs ^long start ^long end]
    (cond 
      (= (long 1) (- end start)) (java.util.Collections/singleton (aget xs start))
      ;;(same-enums? xs start end) (enum-set xs start end)
      :else (immutable-set xs start end))))
;;------------------------------------------------------------------------------
