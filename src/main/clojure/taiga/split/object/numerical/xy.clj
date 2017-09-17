(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2017-01-05"
      :doc "Greedy decision tree splitting." }
    
    taiga.split.object.numerical.xy
  
  (:require [zana.api :as z]
            [taiga.utils :as utils]
            [taiga.tree.split.numerical :as numerical])
  (:import [java.util List]
           [clojure.lang IFn IFn$OD IFn$OL IFn$OOOD Keyword]
           [zana.java.accumulator Accumulator]
           [taiga.java.split.object XY]))
;;------------------------------------------------------------------------------
(defn allocate-cache 
  ([n] (make-array XY n))
  ([^IFn y ^List data]
    (let [n (z/count data)
          ^objects xys (allocate-cache n)]
      (dotimes [i n] (aset xys i ^XY (XY. Double/NaN (.invoke y (.get data i)))))
      xys)))
;;------------------------------------------------------------------------------
(defn split [options]
  (let [^IFn cost-factory (:cost-factory options)
        ^IFn feasible? (:feasible? options)
        ^IFn y (:ground-truth options)
        [^Keyword kx ^IFn$OD x] (:this-predictor options)
        ;; data is mutable!
        ^objects xy-count (XY/cacheXY x y (:data options) (:xys options))
        ^objects xy (aget xy-count 0)
        n (int (aget xy-count 1))]
    (assert (> n 0))
    ;; TODO: this is wrong --- it's assuming that feasible? depends on :mincount
    ;; TODO: move test to bud/split
    (if (> (* 2 (int (:mincount options))) n)
      {:cost Double/POSITIVE_INFINITY}
      (let [^Accumulator cost0 (cost-factory)
            ^Accumulator cost1 (cost-factory)]
        
        (dotimes [i n] (.add cost1 (.y ^XY (aget xy i))))
        
        (loop [i (int 0)
               xmin Double/NEGATIVE_INFINITY
               cmin (.invokePrim ^IFn$OOOD utils/feasible-cost feasible? cost0 cost1)
               nleft (int 0)]
          (let [^XY xyi (aget xy i)
                xi (.x xyi)
                yi (.y xyi)
                _ (.add cost0 yi) 
                _ (.delete cost1 yi)
                ;; move all tied x values from right to left
                j  (int 
                     (loop [jj (int (+ i 1))]
                      (if (>= jj n)
                        jj
                        (let [^XY xyj (aget xy jj)
                              xj (.x xyj)]
                          (if-not (== xi xj)
                            jj
                            (let [yj (.y xyj)]
                              (.add cost0 yj) 
                              (.delete cost1 yj)
                              
                              (recur (+ jj 1))))))))]

            (if (< j n)
              (let [cost (.invokePrim ^IFn$OOOD utils/feasible-cost feasible? cost0 cost1)]
                (if (< cost cmin)
                  (recur (int j) (.x ^XY (aget xy i)) cost (int (.netCount cost0)))
                  (recur (int j) xmin cmin nleft)))
              ;; done
              (if (and (> xmin Double/NEGATIVE_INFINITY)
                       (< cmin Double/POSITIVE_INFINITY))
                {:split (numerical/make kx xmin nleft n) :cost cmin}
                {:cost Double/POSITIVE_INFINITY}))))))))
;;------------------------------------------------------------------------------
