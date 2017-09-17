(set! *warn-on-reflection* true)
(set! *unchecked-math* false) ;; warnings in cheshire.generate
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2017-01-13"
      :doc "A primitive double array-valued decision tree leaf." }
    
    taiga.tree.leaf.doubles
  
  (:require [cheshire.generate]
            [zana.api :as z]
            [taiga.tree.node])
  (:import [taiga.tree.node Node]))
(set! *unchecked-math* :warn-on-boxed)
;;------------------------------------------------------------------------------
;; leaf for vector-valued regression.
;; TODO: keep count of training records that end up in this leaf.
;; TODO: does this really need to implement Node?

(deftype Leaf [^doubles value]
  
  Node
  (isLeaf [this] true)
  (trueChild [this] (throw (UnsupportedOperationException.)))
  (falseChild [this] (throw (UnsupportedOperationException.)))
  (child [this predictors datum] (throw (UnsupportedOperationException.)))
  (extract [this predictors] (throw (UnsupportedOperationException.)))
  
  (withChildren [this children] (assert (z/empty? children)) this)
  (isFertile [this] false)
  (getChildren [this] nil)
  
  clojure.lang.IFn
  (invoke [this predictors datum] value)
  
  Object 
  (equals [this that] 
    (and 
      (instance? Leaf that) 
      (let [^doubles v1 (.value ^Leaf that)]
        (or (identical? value v1)
            (java.util.Arrays/equals value v1))))))
;;------------------------------------------------------------------------------
(defn map->Leaf [m] (Leaf. (:value m)))

(defn map<-Leaf [^Leaf l] {:class :leaf :value (.value l)})

(defmethod z/clojurize Leaf [this] (map<-Leaf this))

(defmethod print-method Leaf [^Leaf this ^java.io.Writer w]
  (if *print-readably*
    (do
      (.write w " #taiga.tree.leaf.doubles.Leaf{:value ")
      (.write w (pr-str (.value this)))
      (.write w "} "))
    (.write w (print-str (map<-Leaf this)))))
;;------------------------------------------------------------------------------
;; EDN input (output just works)
;;------------------------------------------------------------------------------
(z/add-edn-readers! {'taiga.tree.leaf.doubles.Leaf map->Leaf})
;;------------------------------------------------------------------------------
;; JSON output (input not supported)
;;------------------------------------------------------------------------------
(defn- leaf-encoder [^Leaf l json-generator]
  (cheshire.generate/encode-map (map<-Leaf l) json-generator))
(cheshire.generate/add-encoder Leaf leaf-encoder)
;;------------------------------------------------------------------------------
;; learners
;;------------------------------------------------------------------------------
(defn mean-learner [ground-truth dimension]
  (assert (instance? clojure.lang.IFn ground-truth))
  (let [^clojure.lang.IFn calculator 
        (z/make-object-calculator #(z/vector-mean-accumulator dimension) 
                                  ground-truth)]
    (fn learn-mean-leaf ^taiga.tree.leaf.doubles.Leaf [data]
      (Leaf. (calculator data)))))
;;------------------------------------------------------------------------------
