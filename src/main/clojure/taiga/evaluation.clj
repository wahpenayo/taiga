(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "Kristina Lisa Klinkner, John Alan McDonald" :date "2016-11-11"
      :doc "Functions to extract RF details for evaluation." }

    taiga.evaluation
  
  (:require [taiga.ensemble :as addm]
            [taiga.tree.node :as node])
  (:import [taiga.ensemble EnsembleModel]))

(defn tree-sequence [^EnsembleModel model] (seq (.terms model)))

(defn tree-scores [^EnsembleModel model datum]
  (let [trees (tree-sequence model)
        get-score (fn [tree] (tree datum))]
    (map get-score trees)))

(defn all-tree-scores [^EnsembleModel model data]
  (let [all-scores (fn [d] (tree-scores model d))]
    (mapv all-scores data)))

(defn tree-seq-depth
  "Returns a lazy sequence of vectors of the nodes in a tree and their
  depth as [node depth], via a depth-first walk.  branch? must be a fn
  of one arg that returns true if passed a node that can have
  children (but may not).  children must be a fn of one arg that
  returns a sequence of the children. Will only be called on nodes for
  which branch? returns true. Root is the root node of the tree."
  [branch? children root]
  (let [walk (fn walk [^long depth node]
               (lazy-seq
                 (cons [node depth]
                       (when (branch? node)
                         (mapcat (partial walk (inc depth))
                                 (children node))))))]
    (walk 0 root)))

(defn tree-depths [^EnsembleModel model datum]
  (let [trees (tree-sequence model)
        get-depth (fn [tree]
                    (tree-seq-depth node/fertile?  node/children tree))]
    (map get-depth trees)))

(defn all-tree-depths [^EnsembleModel model data]
  (let [all-depths (fn [d] (tree-depths model d))]
    (mapv all-depths data)))