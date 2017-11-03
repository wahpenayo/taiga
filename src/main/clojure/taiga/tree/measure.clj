(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed) 
(ns ^{:author "wahpenayo at gmail dot com" 
      :since "2017-10-30"
      :date "2017-11-02"
      :doc "probability measure prediction." }
    
    taiga.tree.measure
  
  (:require [zana.api :as z]
            [taiga.tree.node :as node])
  
  (:import [java.util HashMap Map]
           [clojure.lang IFn IFn$OD]
           [org.apache.commons.math3.distribution 
            RealDistribution]
           [taiga.tree.node Node]
           [taiga.tree.leaf.double Leaf]))
;;----------------------------------------------------------------
(defn- prediction-function [^Node root 
                            ^Map measures]
  "assumes <code>measures</code> is a map from <code>Leaf</code>
   to <code>RealDistribution</code> and has a value
   for every leaf under <code>root</code>."
  (fn ^RealDistribution [^Map predictors
                         ^Object datum]
    (let [^Leaf leaf (node/leaf root predictors datum)
          ^RealDistribution rpm (.get measures leaf)]
      (assert (not (nil? rpm)))
      rpm)))
;;----------------------------------------------------------------
;; TODO: should this return an instance of some prediciton model
;; class, rather than just a simple function? 
;; So that it can be examined?
(defn train
  
  "Wrap a decision tree with a prediction function, whose values 
   will be probability measures (instances of 
   <code>RealDistribution</code>).
   Arguments to the prediction function will be:
   <dl>
   <dt><code>predictors</code></dt>
   <dd>a <code>java.util.Map</code> from keywords to functions that can be
   applied to the elements of <code>data</code>. Must have key-value 
   (keyword, function) pairs for the split attributes encountered between 
   <code>node</code> and the resulting leaf.</dd>
   <dt><code>datum</code></dt>
   <dd>an object to which the predictor functions can be applied.</dd>
   </dl>.
   The returned probability measure will be estimated from the
   <code>training-data</code> that ends up in the same leaf as the
   later <code>datum</code> argument.<br>
   <b>NOTE:</b> if a leaf gets no empirical distribution training 
   data, the map will return <code>nil</code>."
  
  ^clojure.lang.IFn [^Node root 
                     ^IFn$OD ground-truth
                     ^Map predictors 
                     ^Iterable training-data]
  (let [leaf-ground-truth (node/leaf-ground-truth 
                            root ground-truth predictors 
                            training-data)
        leaf-to-measure (HashMap.)]
    (z/mapc (fn collect-measures [^Node l]
              (let [^doubles ldata (.get leaf-ground-truth l)]
                (.put leaf-to-measure l (z/make-wepdf ldata))))
            (.keySet leaf-ground-truth))
    (prediction-function root leaf-to-measure)))
;;----------------------------------------------------------------
