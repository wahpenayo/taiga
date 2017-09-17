(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2017-01-03"
      :doc "Brute force search over all 2-way partitions of the categories." }
    
    taiga.split.object.categorical.all-subsets
  
  (:require [zana.api :as z]
            [taiga.utils :as utils]
            [taiga.split.object.categorical.move :as move]
            [taiga.tree.split.categorical :as categorical])
  (:import [java.util Collection]
           [zana.java.accumulator Accumulator]))
;;------------------------------------------------------------------------------
(defn- sum-counts ^long [^java.util.Map groups cats]
  (loop [cats (seq cats)
         sum (long 0)]
    (if-not (empty? cats)
      (recur (rest cats) (+ sum (z/count (.get groups (first cats)))))
      sum)))
;;------------------------------------------------------------------------------
;; Try all possible binary partitions using a method that moves only one
;; category from right to left, or vice versa, at a time, and moves the
;; smallest cardinality categories most and highest cardinality categories
;; least. 
(defn split [options]
  (let [^clojure.lang.IFn cost-factory (:cost-factory options)
        ^clojure.lang.IFn feasible? (:feasible? options)
        [^clojure.lang.Keyword kx ^clojure.lang.IFn x] (:this-predictor options)
        ^java.util.Map groups (z/group-by-not-nil x (:data options))]
    (if (== 1 (z/count groups))
      {:cost Double/POSITIVE_INFINITY}
      (let [^clojure.lang.IFn y (:ground-truth options)
            ^Accumulator cost0 (cost-factory)
            ^Accumulator cost1 (cost-factory)
            cats (seq (z/sort-by #(z/count (.get groups %)) (z/keys groups)))
            left (set (butlast cats))
            c1 (last cats)
            right #{c1}]
        (doseq [c left] 
          (let [^java.util.List group (.get groups c)
                n (.size group)]
            (dotimes [i n] (.add cost0 (y (.get group i))))))
        (let [^java.util.List group (.get groups c1)
              n (.size group)]
          (dotimes [i n] (.add cost1 (y (.get group i)))))
        (loop [moves (move/moves left)
               left left
               right right
               cmin (double (utils/feasible-cost feasible? cost0 cost1))
               lmin left
               rmin right]
          (if-not (empty? moves)
            (let [move (first moves)
                  c (move/item move)
                  right? (move/right? move)
                  ^java.util.List group (.get groups c)
                  n (.size group)
                  _ (if right?
                      (dotimes [i n]
                        (let [yi (y (.get group i))]
                          (.delete cost0 yi) 
                          (.add cost1 yi)))
                      (dotimes [i n]
                        (let [yi (y (.get group i))]
                          (.add cost0 yi) 
                          (.delete cost1 yi))))
                  cost (double (utils/feasible-cost feasible? cost0 cost1))
                  left (if right? (disj left c) (conj left c))
                  right (if right? (conj right c) (disj right c))]
              (if (< cost cmin)
                (recur (rest moves) left right cost left right)
                (recur (rest moves) left right cmin lmin rmin)))
            (if (or (>= cmin Double/POSITIVE_INFINITY)
                    (empty? lmin)
                    (empty? rmin))
              {:cost cmin}
              ;; default (not in cats) is majority vote direction
              (let [nleft (sum-counts groups lmin)
                    nright (sum-counts groups rmin)
                    cats (utils/to-set (if (> nleft nright) rmin lmin))]
                {:split (categorical/make kx cats) :cost cmin}))))))))
;;------------------------------------------------------------------------------
