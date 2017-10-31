(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed) 
(ns ^{:author ["John Alan McDonald" 
               "Kristina Lisa Klinkner"
               "palisades dot lakes at gmail dot com"]
      :since "2016-11-11"
      :date "2017-10-30"
      :doc "deftype/clojure.zipper representation of decision trees." }
    
    taiga.tree.node
  
  (:require [zana.api :as z])
  
  (:import [java.util ArrayList HashMap List Map]
           [clojure.lang IFn$OD]
           [com.carrotsearch.hppc DoubleArrayList]))
;;----------------------------------------------------------------
;; decision tree representation with clojure.zipper support
;;----------------------------------------------------------------
;; Nodes are also functions. Invoking a node on a datum descends to a leaf
;; and invokes that leaf on the datum.
(definterface Node
  (^boolean isLeaf [])
  ;; nil for leaves
  (^taiga.tree.node.Node trueChild [])
  (^taiga.tree.node.Node falseChild [])
  (^taiga.tree.node.Node child [predictors datum])
  #_(^taiga.tree.node.Node leaf [predictors datum])
  ;; return a function that can be applied directly to datum instances for 
  ;; faster splitting of data sets.
  (^clojure.lang.IFn extract [^java.util.Map predictors])
  ;; zipper api
  (^taiga.tree.node.Node withChildren [children])
  (^taiga.tree.node.Node withChildren [^taiga.tree.node.Node child0 
                                       ^taiga.tree.node.Node child1])
  (^boolean isFertile [])
  (^Iterable getChildren []))
;;----------------------------------------------------------------
;; Functional API
;; TODO: do we need these?
;;----------------------------------------------------------------
(defn leaf? [^Node node] (.isLeaf node))
(defn true-child ^taiga.tree.node.Node [^Node node] (.trueChild node))
(defn false-child ^taiga.tree.node.Node [^Node node] (.falseChild node))
;;----------------------------------------------------------------
(defn with-children ^taiga.tree.node.Node [^Node node children]
  (.withChildren node children))
(defn fertile? [^Node node] (.isFertile node))
(defn children ^Iterable [^Node node] (.getChildren node))
;;----------------------------------------------------------------
(defn child ^taiga.tree.node.Node 
            [^Node node ^java.util.Map predictors datum path]
  (.child node predictors datum))
;;----------------------------------------------------------------
;; interface dependent functions
;;----------------------------------------------------------------
(defn leaf 
  
  "A method in the taiga.tree.Node protocol.<br>
   Returns the <code>taiga.tree.leaf.double.Leaf</code> resulting from passing 
   <code>datum</code> down the decision tree rooted at <code>node</code>.
   <dl>
   <dt><code>node</code></dt>
   <dd>root <code>taiga.tree.Node</code></dd>
   <dt><code>predictors</code></dt>
   <dd>a <code>java.util.Map</code> from keywords to functions that can be
   applied to the elements of <code>data</code>. Must have key-value 
   (keyword, function) pairs for the split attributes encountered between 
   <code>node</code> and the resulting leaf.</dd>
   <dt><code>datum</code></dt>
   <dd>an object to which the predictor functions can be applied.</dd>
   </dl>"
  
  ^clojure.lang.IFn$OOD [^Node node ^java.util.Map predictors datum]
  (let [^Node c (.child node predictors datum)]
    (if (.isLeaf c)
      c
      (recur c predictors datum))))

(defn branch ^Iterable [^Node node ^java.util.Map predictors datum path]
  (let [path (conj path node)
        ^Node c (.child node predictors datum)]
    (if (.isLeaf c)
      path
      (recur c predictors datum path))))
;;----------------------------------------------------------------
;; TODO: function that returns all the leaves?
;;----------------------------------------------------------------
;; tree statistics
;;----------------------------------------------------------------
;;
(defn leaf-data
  
  "Return a map from leaf node to List, where
   the list contains the data that ends up in that leaf." 
  
  ^java.util.Map [^Node root 
                  ^Map predictors 
                  ^Iterable data]
  (let [^Map leaf-to-data (HashMap. (z/count data))]
    (z/mapc (fn update-map [datum]
              (let [^Node l (leaf root predictors datum)
                    _(assert (not (nil? l)))
                    ^List ldata (.getOrDefault leaf-to-data l (ArrayList.))]
                (.add ldata datum)
                (.put leaf-to-data l ldata)))
            data)
    leaf-to-data))
;;----------------------------------------------------------------
;; TODO: what about leaves that get no data?
(defn leaf-ground-truth
  
  "Return a map from leaf node to double[], where
   the list contains the ground truth values for the data that
   ends up in that leaf.
   Only for double-valued ground truth for now." 
  
  ^java.util.Map [^Node root 
                  ^Map predictors 
                  ^Iterable data]
  (let [^Map leaf-to-data (HashMap. (z/count data))
        ^IFn$OD y (:ground-truth predictors)]
    (assert (not (nil? y)))
    (z/mapc (fn update-map [datum]
              (let [^Node l (leaf root predictors datum)
                    _(assert (not (nil? l)))
                    ^DoubleArrayList ldata (.getOrDefault leaf-to-data l (DoubleArrayList.))]
                (.add ldata (.invokePrim y datum))
                (.put leaf-to-data l ldata)))
            data)
    (z/mapc (fn list-to-array [^Node l]
              (let [^DoubleArrayList ldata (.get leaf-to-data l)]
                (.put leaf-to-data l (.toArray ldata))))
            (.keySet leaf-to-data))
    leaf-to-data))
;;----------------------------------------------------------------
(defn count-leaves 
  
  "How many leaves in the subtree rooted at <code>node</code>?
   <dl>
   <dt><code>node</code></dt><dd><code>Node</code></dd>
   </dl>"
  
  ^long [^Node node]
  
  (assert (instance? Node node) (pr-str (class node) node))
  (if (fertile? node)
    (+ (count-leaves (.trueChild node))
       (count-leaves (.falseChild node)))
    1))
;;----------------------------------------------------------------
(defn count-children 
  
  "How many nodes in the subtree rooted at <code>node</code>?
   <br>(Should probably be called <code>tree-node-count</code> or something.)
   <dl>
   <dt><code>node</code></dt><dd><code>Node</code></dd>
   </dl>"
  
  ^long [^Node node]
  (assert (instance? Node node)  (pr-str (class node) node))
  (if (fertile? node)
    (+ 1
       (count-children (.trueChild node))
       (count-children (.falseChild node)))
    1))
;;----------------------------------------------------------------
(defn node-height 
  
  "What is the maximum number of nodes in a path from this <code>node</code>
   to a leaf, counting <code>node</code> and the leaf?
   <dl>
   <dt><code>node</code></dt><dd><code>Node</code></dd>
   </dl>"
  
  ^long [^Node node]
  
  (assert (instance? Node node) (pr-str (class node) node))
  (if (fertile? node)
    (+ 1 (max (node-height (.trueChild node))
              (node-height (.falseChild node))))
    1))
;;----------------------------------------------------------------
