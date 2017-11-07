(set! *warn-on-reflection* true)
(set! *unchecked-math* false) ;; warnings in cheshire.generate
(ns ^{:author ["wahpenayo at gmail dot com"
               "John Alan McDonald" 
               "Kristina Lisa Klinkner"]
      :since "2016-11-29"
      :date "2017-11-06"
      :doc "A general object-valued decision tree leaf." }
    
    taiga.tree.leaf.object
  
  (:require [cheshire.generate]
            [zana.api :as z]
            [taiga.tree.node]
            [taiga.tree.bud])
  (:import [taiga.tree.bud Bud]
           [taiga.tree.node Node]))
(set! *unchecked-math* :warn-on-boxed)
;;------------------------------------------------------------------------------
;; leaf for multi-class classification regression.
;; TODO: Enum version
;; TODO: keep count of training records that end up in this leaf.
;; TODO: does this really need to implement Node?

(deftype Leaf [value]
  
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
  (hashCode [_] 
    (unchecked-add-int (unchecked-multiply-int 17 31)
                       (.hashCode value)))
  (equals [this that] 
    (and 
      (instance? Leaf that) 
      (let [v1 (.value ^Leaf that)]
        (or (identical? value v1)
            (= value v1))))))
;;------------------------------------------------------------------------------
(defn map->Leaf [m] (Leaf. (:value m)))

(defn map<-Leaf [^Leaf l] {:class :leaf :value (.value l)})

(defmethod z/clojurize Leaf [this] (map<-Leaf this))

(defmethod print-method Leaf [^Leaf this ^java.io.Writer w]
  (if *print-readably*
    (do
      (.write w " #taiga.tree.leaf.object.Leaf{:value ")
      (.write w (print-str (.value this)))
      (.write w "} "))
    (.write w (print-str (map<-Leaf this)))))
;;------------------------------------------------------------------------------
;; EDN input (output just works)
;;------------------------------------------------------------------------------
(z/add-edn-readers! {'taiga.tree.leaf.double.Leaf map->Leaf})
;;------------------------------------------------------------------------------
;; JSON output (input not supported)
;;------------------------------------------------------------------------------
(defn- leaf-encoder [^Leaf l json-generator]
  (cheshire.generate/encode-map (map<-Leaf l) json-generator))
(cheshire.generate/add-encoder Leaf leaf-encoder)
;;------------------------------------------------------------------------------
