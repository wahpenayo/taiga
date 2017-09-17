(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2016-11-29"
      :doc "deftype/clojure.zipper representation of decision trees." }
    
    taiga.tree.grow
  
  (:require [clojure.zip :as zip]
            [zana.api :as z]
            [taiga.tree.node :as node]
            [taiga.tree.bud :as bud])
  (:import [taiga.tree.bud Bud]))
;;------------------------------------------------------------------------------
;; clojure.zipper interface
;;------------------------------------------------------------------------------
(defn- zipper [root]
  (assert (instance? Bud root) (class root))
  (zip/zipper node/fertile? node/children node/with-children root))
;;------------------------------------------------------------------------------
(defn- split-loc [loc]
  (let [bud (zip/node loc)
        new-node (bud/sprout bud)]
    (assert (instance? Bud bud) (class bud))
    (zip/replace loc (zip/make-node loc new-node (node/children new-node)))))
;;------------------------------------------------------------------------------
(defn- grow-loc [loc]
  (cond (zip/end? loc) (zip/root loc)
        (zip/branch? loc) (recur (zip/next loc))
        :else (recur (zip/next (split-loc loc)))))
;;------------------------------------------------------------------------------
;; returns a function that uses a tree to do prediction
(defn learn [maxdepth leaf-learner split-learner predictors data]
  (grow-loc 
    (zipper 
      (bud/make maxdepth leaf-learner split-learner predictors 
                (java.util.ArrayList. ^java.util.Collection data)))))
;;------------------------------------------------------------------------------
