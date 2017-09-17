(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-12-21"
      :doc "Greedy decision tree splitting." }
    
    taiga.split.numerical.numerical.xy
  
  (:require [taiga.utils :as utils]
            [taiga.tree.split.numerical :as numerical])
  (:import [java.util List]
           [clojure.lang IFn IFn$OD IFn$OL Keyword]
           [zana.java.accumulator Accumulator]
           [taiga.java.split.numerical XY]))
;;------------------------------------------------------------------------------
(defn allocate-cache [n] (make-array XY n))
;;------------------------------------------------------------------------------
(defn split [options]
  (let [^IFn cost-factory (:cost-factory options)
        ^IFn feasible? (:feasible? options)
        ^IFn$OD y (:ground-truth options)
        _(assert (instance? IFn$OD y))
        [^Keyword kx ^IFn$OD x] (:this-predictor options)
        _(assert (instance? IFn$OD x))
        ;; data is mutable!
        ^objects data-count (XY/cacheXY x y (:data options) (:xys options))
        ^objects data (aget data-count 0)
        n (int (aget data-count 1))]
    (assert (> n 0))
    ;; TODO: this is wrong --- it's assuming that feasible? depends on :mincount
    ;; TODO: move test to bud/split
    (if (or (> (* 2 (int (:mincount options))) n))
      {:cost Double/POSITIVE_INFINITY}
      (let [^Accumulator cost0 (cost-factory)
            ^Accumulator cost1 (cost-factory)]
        (dotimes [i n] (.add cost1 (.y ^XY (aget data i))))
        (loop [i (int 0)
               xmin Double/NEGATIVE_INFINITY
               cmin (double (utils/feasible-cost feasible? cost0 cost1))
               nleft (int 0)]
          (let [^XY di (aget data i)
                xi (.x di)
                yi (.y di)
                _ (.add cost0 yi) 
                _ (.delete cost1 yi)
                ;; move all tied x values from right to left
                j  (int 
                     (loop [jj (int (+ i 1))]
                      (if (>= jj n)
                        jj
                        (let [^XY dj (aget data jj)
                              xj (.x dj)]
                          (if-not (== xi xj)
                            jj
                            (let [yj (.y dj)]
                              (.add cost0 yj) 
                              (.delete cost1 yj)
                              (recur (inc jj))))))))]
            (if (< j n)
              (let [cost (double (utils/feasible-cost feasible? cost0 cost1))]
                (if (< cost cmin)
                  (recur (int j) (.x ^XY (aget data i)) cost (int (.netCount cost0)))
                  (recur (int j) xmin cmin nleft)))
              ;; done
              (if (and (> xmin Double/NEGATIVE_INFINITY)
                       (< cmin Double/POSITIVE_INFINITY))
                {:split (numerical/make kx xmin nleft n) :cost cmin}
                {:cost Double/POSITIVE_INFINITY}))))))))
;;------------------------------------------------------------------------------
