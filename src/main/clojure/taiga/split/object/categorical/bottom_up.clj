(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2016-12-19"
      :doc 
      "Use min cost increase hierarchical clustering to generate a binary tree 
       of categories. The heuristic split is the 2 children of the root." }
    
    taiga.split.object.categorical.bottom-up
  
  (:require [zana.api :as z]
            [taiga.utils :as utils]
            [taiga.split.object.categorical.cluster :as cluster]
            [taiga.tree.split.categorical :as categorical])
  (:import [taiga.split.object.categorical.cluster Cluster]))
;;------------------------------------------------------------------------------
;; TODO: should the cost-factory enclose the y attribute?
;;------------------------------------------------------------------------------
;; all possible merges of the leaves of the clustering tree

(defn- initialize-clusters [x y data cost-factory feasible?]
  (let [groups (z/group-by-not-nil x data)]
    (map #(cluster/make (java.util.Collections/singleton %)
                        (.get groups %) nil nil y cost-factory feasible?) 
         (z/keys groups))))
;;------------------------------------------------------------------------------
;; all possible merges of the leaves of the clustering tree
;; TODO: should merges be a Set or SortedSet?

(defn- all-merges [^java.util.List clusters y cost-factory feasible?]
  (let [n  (.size clusters)
        merges (java.util.ArrayList. (int (quot (* n (dec n)) 2)))]
    (loop [i (int 0)]
      (when (< i n)
        (let [ci (.get clusters i)]
          (loop [j (inc i)]
            (when (< j n)
              (.add merges 
                (cluster/merge ci (.get clusters j) y cost-factory feasible?))
              (recur (inc j)))))
        (recur (inc i))))
    merges))
;;------------------------------------------------------------------------------
(defn- all-possible [x y data cost-factory feasible?]
  (let [clusters (initialize-clusters x y data cost-factory feasible?)]
    [clusters (all-merges clusters y cost-factory feasible?)]))
;;------------------------------------------------------------------------------
;; find the best merge in the list

(defn- optimal-merge [^Iterable merges]
  (let [im (.iterator merges)]
    (loop [^Cluster optimal nil
           mincost Double/POSITIVE_INFINITY]
      (if (.hasNext im)
        (let [^Cluster merge (.next im)
              cost (cluster/merge-cost merge)]
          (if (< cost mincost)
            (recur merge cost)
            (recur optimal mincost)))
        optimal))))
;;------------------------------------------------------------------------------
;; do the optimal merge

(defn- one-merge [clusters merges y cost-factory feasible?]
  (let [optimal (optimal-merge merges)
        left (cluster/left optimal)
        right (cluster/right optimal)
        merges (remove (fn [^Cluster m] 
                         (or (= left (cluster/left m))
                             (= left (cluster/right m))
                             (= right (cluster/left m))
                             (= right (cluster/right m))))
                       merges)
        clusters (remove #(or (= left %) (= right %)) clusters)
        optimal (cluster/forget-children optimal)
        new-merges (map #(cluster/merge optimal % y cost-factory feasible?) 
                        clusters)]
    [(conj clusters optimal) (concat new-merges merges)]))
;;------------------------------------------------------------------------------
(defn- infeasible? [^Cluster c]
  (not (and (cluster/feasible-leaf? c)
            (cluster/feasible-leaf? (cluster/left c))
            (cluster/feasible-leaf? (cluster/right c)))))
;;------------------------------------------------------------------------------
;; Kristina's suggestion: first optimize over just those merges that involve
;; clusters that would be infeasible leaves (eg too few training case), until 
;; there aren't any more.

(defn- feasible-clusters [[clusters merges] y cost-factory feasible?]
  (let [merges (filter infeasible? merges)
        nclusters (count clusters)]
    (cond 
      (or (empty? merges) (== 1 nclusters)) clusters
      
      (< 2 nclusters) (recur 
                        (one-merge clusters merges y cost-factory feasible?)
                        y cost-factory feasible?)
      
      :else (throw (IllegalStateException. "shouldn't get here!")))))
;;------------------------------------------------------------------------------
(defn- make-split [kx clusters]
  (assert (== 2 (count clusters)))
  (let [[^Cluster c0 ^Cluster c1] clusters
        n0 (z/count (cluster/data c0))
        n1 (z/count (cluster/data c1))
        cats (utils/to-set (if (> n0 n1) 
                             (cluster/categories c1) 
                             (cluster/categories c0)))]
    {:split (categorical/make kx cats) 
     :cost (+ (cluster/cost c0) (cluster/cost c1))}))
;;------------------------------------------------------------------------------
(defn split [options]
  (let [cost-factory (:cost-factory options)
        feasible? (:feasible? options)
        y (:ground-truth options)
        [kx x] (:this-predictor options)
        data (:data options)]
    (loop [clusters (feasible-clusters 
                      (all-possible x y data cost-factory feasible?)
                      y cost-factory feasible?)
           merges (all-merges clusters y cost-factory feasible?)]
      (let [nclusters (count clusters)]
        (cond 
          ;; singular case, should only happen on first iteration
          (== 1 nclusters) {:cost Double/POSITIVE_INFINITY}
          ;; done
          (== 2 nclusters) (make-split kx clusters)
          (< 2 nclusters) (let [[clusters merges] 
                                (one-merge clusters merges y cost-factory feasible?)]
                            (recur clusters merges))
          :else (throw (IllegalStateException. "shouldn't get here!")))))))
;;------------------------------------------------------------------------------
