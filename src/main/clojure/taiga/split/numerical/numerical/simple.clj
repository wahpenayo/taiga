(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2016-12-16"
      :doc "Greedy decision tree splitting." }
    
    taiga.split.numerical.numerical.simple
  
  (:require [zana.api :as z]
            [taiga.utils :as utils]
            [taiga.tree.split.numerical :as numerical])
  (:import [clojure.lang IFn IFn$OD IFn$OL Keyword]
           [zana.java.arrays Sorter Arrays]
           [zana.java.accumulator Accumulator]))
;;------------------------------------------------------------------------------
(defn- extract-arrays-xy [options]
  (let [^java.util.List data (:data options)
        ^doubles x0 (:x0 options)
        ^doubles y0 (:y0 options)
        [_ x] (:this-predictor options)
        ^IFn$OD y (:ground-truth options)]
    (cond (instance? IFn$OD x) (Arrays/filterNaNs ^IFn$OD x y data x0 y0)
          (instance? IFn$OL x) (Arrays/filterNaNs ^IFn$OL x y data x0 y0)
          :else (throw 
                  (IllegalArgumentException.
                  (pr-str 
                    "Don't know how to filterNaNs when x is:" (class x)))))))
;;------------------------------------------------------------------------------
(defn split [options]
  (let [cost-factory (:cost-factory options)
        feasible? (:feasible? options)
        [kx _] (:this-predictor options)
        [^long n ^doubles x0 ^doubles y0] (extract-arrays-xy options)
        n-1 (int (- n 1))]
    ;; TODO: this is wrong --- it's assuming that feasible? depends on :mincount
    ;; TODO: move test to bud/split
    (if (or (> (* 2 (int (:mincount options))) n)
            ;; singular xs should be filtered already
            #_(z/singular? x0)
            ;; shouldn't get here is y is singular
            #_(z/singular? y0))
      {:cost Double/POSITIVE_INFINITY}
      (let [^Accumulator cost0 (cost-factory)
            ^Accumulator cost1 (cost-factory)]
        (Sorter/quicksort x0 y0 (int 0) (int n))
        (dotimes [i (int n)] (.add cost1 (aget y0 i)))
        (loop [i (int 0)
               xmin Double/NEGATIVE_INFINITY
               cmin (double (utils/feasible-cost feasible? cost0 cost1))
               nleft (int 1)]
          (let [j (int
                    (if (>= i n-1)
                      i
                      (let [xi (aget x0 i)]
                        (loop [j i]
                          (if (and (< j n) (== xi (aget x0 j)))
                            (let [yj (aget y0 j)]
                              (.delete cost1 yj)
                              (.add cost0 yj)
                              (recur (inc j)))
                            j)))))]
            (if (< j n-1)
              (let [cost (double (utils/feasible-cost feasible? cost0 cost1))]
                (if (< cost cmin)
                  (recur (int j) (aget x0 i) cost (int (.netCount cost0)))
                  (recur (int j) xmin cmin nleft)))
              ;; done
              (if (and (> xmin Double/NEGATIVE_INFINITY)
                       (< cmin Double/POSITIVE_INFINITY))
                {:split (numerical/make kx xmin nleft n) :cost cmin}
                {:cost Double/POSITIVE_INFINITY}))))))))
;;------------------------------------------------------------------------------
