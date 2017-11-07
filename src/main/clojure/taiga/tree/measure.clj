(set! *warn-on-reflection* true)
(set! *unchecked-math* false) ;; warnings in cheshire.generate
(ns ^{:author "wahpenayo at gmail dot com" 
      :since "2017-10-30"
      :date "2017-11-06"
      :doc "probability measure prediction." }
    
    taiga.tree.measure
  
  (:require [cheshire.generate]
            [zana.api :as z]
            [taiga.tree.node :as node])
  
  (:import [java.util HashMap Map]
           [clojure.lang IFn IFn$OD]
           [org.apache.commons.math3.distribution 
            RealDistribution]
           [taiga.tree.node Node]
           [taiga.tree.leaf.double Leaf]))
(set! *unchecked-math* :warn-on-boxed)
;;----------------------------------------------------------------
;; need to define a class, rather than just using a closure 
;; function; because Clojure functions don't serialize easily to
;; JSON/EDN/...
(deftype ProbabilityMeasurePrediction [^Node root 
                                       ^Map measures]
  IFn 
  (invoke [_ predictors datum]
    (let [^Map predictors predictors
          ^Leaf leaf (node/leaf root predictors datum)
          ^RealDistribution rpm (.get measures leaf)]
      (assert (not (nil? rpm)))
      rpm))
  
  Object
  (hashCode [_]
    (let [result (int 17)
          result (unchecked-add-int 
                   (unchecked-multiply-int (int 31) result)
                   (.hashCode root))
          result (unchecked-add-int 
                   (unchecked-multiply-int (int 31) result)
                   (.hashCode measures))]
      result))
  (equals [_ that]
    (and (instance? ProbabilityMeasurePrediction that)
         (let [^ProbabilityMeasurePrediction that that]
           (and (.equals root (.root that))
                (.equals measures (.measures that)))))))
;;----------------------------------------------------------------
(defn- prediction-function [^Node root 
                            ^Map measures]
  "assumes <code>measures</code> is a map from <code>Leaf</code>
   to <code>RealDistribution</code> and has a value
   for every leaf under <code>root</code>."
  (assert (not (nil? root)))
  (assert (instance? Node root)
          (print-str (class root) root))
  (assert (and measures 
               (instance? Map measures)
               (every? #(instance? Leaf %) 
                       (keys measures))
               (every? #(instance? RealDistribution %) 
                       (vals measures))))
  (ProbabilityMeasurePrediction. root measures))
;;----------------------------------------------------------------
;; text serialization
;;----------------------------------------------------------------
;; TODO: EDN and JSON serialization for general RealDistributions
(defn map->ProbabilityMeasurePrediction 
  ^ProbabilityMeasurePrediction [^Map m]
  (prediction-function (:root m) (:measures m)))
(defn map<-ProbabilityMeasurePrediction 
  ^Map [^ProbabilityMeasurePrediction pmp]
  {:root (.root pmp)
   :measures (.measures pmp)})
(defmethod z/clojurize ProbabilityMeasurePrediction [this] 
  (map<-ProbabilityMeasurePrediction this))
(defmethod print-method 
  ProbabilityMeasurePrediction
  [^ProbabilityMeasurePrediction this 
   ^java.io.Writer w]
  (if *print-readably*
    (do
      (.write w 
        " #taiga.tree.measure.ProbabilityMeasurePrediction{:root ")
      (.write w (print-str (.root this)))
      (.write w " :measures ")
      (.write w (print-str (.measures this)))
      (.write w "} "))
    (.write w 
      (print-str (map<-ProbabilityMeasurePrediction this)))))
;;----------------------------------------------------------------
;; EDN input (output just works)
;;----------------------------------------------------------------
(z/add-edn-readers! 
  {'taiga.tree.measure.ProbabilityMeasurePrediction 
   map->ProbabilityMeasurePrediction})
;;----------------------------------------------------------------
;; JSON output (input not supported)
;;----------------------------------------------------------------
(defn- ProbabilityMeasurePrediction-encoder 
  [^ProbabilityMeasurePrediction pmp json-generator]
  (cheshire.generate/encode-map 
    (map<-ProbabilityMeasurePrediction pmp)
    json-generator))
(cheshire.generate/add-encoder 
  ProbabilityMeasurePrediction 
  ProbabilityMeasurePrediction-encoder)
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
   <dd>a <code>java.util.Map</code> from keywords to functions 
       that can be applied to the elements of <code>data</code>. 
       must have key-value (keyword, function) pairs for the split 
       attributes encountered between <code>node</code> and the 
       resulting leaf.
   </dd>
   <dt><code>datum</code></dt>
   <dd>an object to which the predictor functions can be applied.
   </dd>
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
