(set! *warn-on-reflection* true)
(set! *unchecked-math* false)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2016-11-29"
      :doc "Greedy decision tree splitting." }
    
    taiga.tree.split.categorical
  
  (:require [clojure.string :as s]
            [cheshire.generate]
            [zana.api :as z]
            [taiga.utils :as utils]
            [taiga.tree.node :as node]))
(set! *unchecked-math* :warn-on-boxed)
;;------------------------------------------------------------------------------
(deftype CategoricalSplit [^clojure.lang.Keyword k
                           ^java.util.Set categories
                           ^taiga.tree.node.Node true-child
                           ^taiga.tree.node.Node false-child]
  taiga.tree.node.Node
  (isLeaf [this] false)
  (trueChild [this] true-child)
  (falseChild [this] false-child)
  (child [this predictors datum]
    (let [^clojure.lang.IFn x (.get ^java.util.Map predictors k)]
      (assert x (print-str "predictor" k "not found in\n" 
                           (str (into {} (map (fn [[k v]] [k (z/name v)])
                                              predictors)))))
      (if (.contains categories (x datum))
        true-child 
        false-child)))
  
  (^clojure.lang.IFn extract [this ^java.util.Map predictors]
    (let [^clojure.lang.IFn x (.get predictors k)]
      (assert x (print-str "predictor" k "not found in\n" 
                           (str (into {} (map (fn [[k v]] [k (z/name v)]) 
                                              predictors)))))
      (fn categorical-split-predicate [datum] 
        (.contains categories (x datum)))))
  
  (withChildren [this child0 child1]
    (CategoricalSplit. k categories child0 child1))

  (withChildren [this children] 
    (.withChildren this (first children) (second children)))
  (isFertile [this] true)
  (getChildren [this] (seq [true-child false-child]))
  
  clojure.lang.IFn$OOD
  (invokePrim [this predictors datum] 
    (let [^clojure.lang.IFn$OOD leaf (node/leaf this predictors datum)] 
      (.invokePrim leaf predictors datum)))

  clojure.lang.IFn
  (invoke [this predictors datum] 
    (let [^clojure.lang.IFn leaf (node/leaf this predictors datum)] 
      (.invoke leaf  predictors datum)))
  
  Object
  (equals [this that]
    (and (instance? CategoricalSplit that)
         (let [^CategoricalSplit that that]
           (= k (.k that))
           (= categories (.categories that))))))
;;------------------------------------------------------------------------------
(defn make ^taiga.tree.split.categorical.CategoricalSplit 
           [^clojure.lang.Keyword k
            ^java.util.Set categories]
  (CategoricalSplit. k categories nil nil))
;;------------------------------------------------------------------------------
(defn map->CategoricalSplit [m] 
  (CategoricalSplit. ^clojure.lang.Keyword (:k m) 
                     ^java.util.Set (:categories m)
                     (:true-child m) 
                     (:false-child m)))

(defn map<-CategoricalSplit [^CategoricalSplit s] 
  {;;:class "taiga.split.types.CategoricalSplit" 
   :k (.k s)
   :categories (utils/to-set (.categories s))
   :true-child (.true-child s)
   :false-child (.false-child s)})

(defmethod z/clojurize CategoricalSplit [^CategoricalSplit this] 
  (z/clojurize (map<-CategoricalSplit this)))

(defmethod print-method 
  CategoricalSplit [^CategoricalSplit this ^java.io.Writer w]
  (if *print-readably*
    (do
      (.write w " #taiga.tree.split.categorical.CategoricalSplit{:k ")
      (.write w (.toString (.k this)))
;      (.write w " :categories #{")
;      (.write w (s/join " " (.categories this)))
;      (.write w "} ")
      (.write w " :categories ")
      (.write w (pr-str (.categories this)))
      (.write w " :true-child ")
      (.write w (pr-str (.true-child this)))
      (.write w " :false-child ")
      (.write w (pr-str (.false-child this)))
      (.write w "} "))
    (.write w (print-str (map<-CategoricalSplit this)))))
;;------------------------------------------------------------------------------
;; EDN input (output just works)
;;------------------------------------------------------------------------------
(z/add-edn-readers! 
  {'taiga.tree.split.categorical.CategoricalSplit map->CategoricalSplit})
;;------------------------------------------------------------------------------
;; JSON output (input not supported)
;;------------------------------------------------------------------------------
(defn encoder [^CategoricalSplit s json-generator]
  (cheshire.generate/encode-map (map<-CategoricalSplit s) json-generator))
(cheshire.generate/add-encoder CategoricalSplit encoder)
;;------------------------------------------------------------------------------
