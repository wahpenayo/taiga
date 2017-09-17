(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2016-12-21"
      :doc "deftype/clojure.zipper representation of decision trees." }
    
    taiga.tree.bud
  
  (:require [zana.api :as z]
            [taiga.tree.node :as node])
  (:import [taiga.tree.node Node]))
;;------------------------------------------------------------------------------
;; Temporary placeholder node used while growing trees
(deftype Bud [^int maxdepth 
              ^clojure.lang.IFn leaf-learner 
              ^clojure.lang.IFn split-learner 
              ^java.util.Map predictors 
              ^Iterable data]
  
  taiga.tree.node.Node
  #_(trueChild [this] (throw (UnsupportedOperationException.)))
  #_(falseChild [this] (throw (UnsupportedOperationException.)))
  #_(extract [this predictors] (throw (UnsupportedOperationException.)))
  #_(withChildren [this children] (assert (z/empty? children)) this)
  (isFertile [this] false)
  (getChildren [this] nil)
  
  #_(child [this predictors datum] (throw (UnsupportedOperationException.))))
;;------------------------------------------------------------------------------
(defn- remove-singular [predictors data]
  (into {} (remove (fn [[_ f]] (z/singular? f data)) predictors)))
;;------------------------------------------------------------------------------
(defn make [maxdepth 
            ^clojure.lang.IFn leaf-learner 
            ^clojure.lang.IFn split-learner 
            ^java.util.Map predictors 
            ^Iterable data]
  (Bud. (int maxdepth) 
        leaf-learner 
        split-learner 
        ;; Remove the singular predictors: they won't be any use further down.
        (remove-singular predictors data) 
        data))
;;------------------------------------------------------------------------------
;; tree growing
;;------------------------------------------------------------------------------
(defn- split-bud ^taiga.tree.node.Node [^Bud bud ^Node split]
  (assert (instance? Bud bud) (class bud))
  (let [maxdepth (dec (.maxdepth bud))
        ^java.util.List data (.data bud)
        ^java.util.Map predictors (.predictors bud)
        ^clojure.lang.IFn predicate (.extract split predictors)
        [left-data right-data] (z/!split-with predicate data)
        ll (.leaf-learner bud)
        sl (.split-learner bud)]
    (.withChildren split
      (make maxdepth ll sl predictors left-data)
      (make maxdepth ll sl predictors right-data))))
;;------------------------------------------------------------------------------
(defn- leaf ^taiga.tree.node.Node [^Bud bud] ((.leaf-learner bud) (.data bud)))
 ;------------------------------------------------------------------------------
(defn sprout ^taiga.tree.node.Node [^Bud bud]
  (if (or (<= (.maxdepth bud) 0)
          (<= (count (.predictors bud)) 0))
    (leaf bud)
    (let [split ((.split-learner bud) (.predictors bud) (.data bud))]
      (if split 
        (split-bud bud split)
        (leaf bud)))))
;;------------------------------------------------------------------------------
