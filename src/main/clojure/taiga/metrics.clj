(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2016-08-24"
      :doc "Accuracy metrics designed for permutation importance measures." }
    
    taiga.metrics
  
  (:require [zana.api :as z]))
;;------------------------------------------------------------------------------
(defn mean-absolute-error 
  
  "A [metric function](1metrics.html).
   Returns the mean absolute difference between
   ground truth and model prediction.
   <dl>
   <dt><code>model</code></dt>
   <dd>a <code>clojure.lang.IFn$OOD</code> that takes an attribute map and a
   datum, one of the elements of <code>data</code>.
   </dd>
   <dt><code>attributes</code></dt><dd>a <code>java.util.Map</code> from
   keywords to functions that can be applied to the elements of 
   <code>data</code>. Must contain <code>:ground-truth</code> as a key.
   </dd>
   <dt><code>data</code></dt><dd>an <code>Iterable</code></dd>
   </dl>"
  
  ^double [^clojure.lang.IFn$OOD model
           ^java.util.Map attributes
           ^Iterable data]
  
  (let [predictors (dissoc attributes :ground-truth :prediction)
        ^clojure.lang.IFn$OD ground-truth (:ground-truth attributes)
        _ (assert (instance? clojure.lang.IFn$OD ground-truth))
        ^clojure.lang.IFn$OD yhat (fn yhat ^double [datum] 
                                    (.invokePrim model predictors datum))]
    (z/mean-absolute-difference ground-truth yhat data)))
;;------------------------------------------------------------------------------
(defn rms-error 
  
  "A [metric function](1metrics.html).
   Returns the square root of the mean squared difference between
   ground truth and model prediction.
   <dl>
   <dt><code>model</code></dt>
   <dd>a <code>clojure.lang.IFn$OOD</code> that takes an attribute map and a
   datum, one of the elements of <code>data</code>.
   </dd>
   <dt><code>attributes</code></dt><dd>a <code>java.util.Map</code> from
   keywords to functions that can be applied to the elements of 
   <code>data</code>. Must contain <code>:ground-truth</code> as a key.
   </dd>
   <dt><code>data</code></dt><dd>an <code>Iterable</code></dd>
   </dl>"
  
  ^double [^clojure.lang.IFn$OOD model
           ^java.util.Map attributes
           ^Iterable data]
  
  (let [predictors (dissoc attributes :ground-truth :prediction)
        ^clojure.lang.IFn$OD ground-truth (:ground-truth attributes)
        _ (assert (instance? clojure.lang.IFn$OD ground-truth))
        ^clojure.lang.IFn$OD yhat (fn yhat ^double [datum] 
                                    (.invokePrim model predictors datum))]
    (z/rms-difference ground-truth yhat data)))
;;------------------------------------------------------------------------------
(defn- true-positive ^clojure.lang.IFn [^clojure.lang.IFn$OOD model
                                        ^java.util.Map attributes]
  (let [predictors (dissoc attributes :ground-truth :prediction)
        ^clojure.lang.IFn$OD ground-truth (:ground-truth attributes)
        _ (assert (instance? clojure.lang.IFn$OD ground-truth))
        ^clojure.lang.IFn$OD yhat (fn yhat ^double [datum] 
                                    (.invokePrim model predictors datum))]
    (fn true-positive [datum] 
      (and (== 1.0 (.invokePrim ground-truth datum))
           (== 1.0 (.invokePrim yhat datum))))))

(defn- true-negative ^clojure.lang.IFn [^clojure.lang.IFn$OOD model
                                        ^java.util.Map attributes]
  (let [predictors (dissoc attributes :ground-truth :prediction)
        ^clojure.lang.IFn$OD ground-truth (:ground-truth attributes)
        _ (assert (instance? clojure.lang.IFn$OD ground-truth))
        ^clojure.lang.IFn$OD yhat (fn yhat ^double [datum] 
                                    (.invokePrim model predictors datum))]
    (fn true-negative [datum] 
      (and (== 0.0 (.invokePrim ground-truth datum))
           (== 0.0 (.invokePrim yhat datum))))))

(defn- false-positive ^clojure.lang.IFn [^clojure.lang.IFn$OOD model
                                         ^java.util.Map attributes]
  (let [predictors (dissoc attributes :ground-truth :prediction)
        ^clojure.lang.IFn$OD ground-truth (:ground-truth attributes)
        _ (assert (instance? clojure.lang.IFn$OD ground-truth))
        ^clojure.lang.IFn$OD yhat (fn yhat ^double [datum] 
                                    (.invokePrim model predictors datum))]
    (fn false-positive [datum] 
      (and (== 0.0 (.invokePrim ground-truth datum))
           (== 1.0 (.invokePrim yhat datum))))))

(defn- false-negative ^clojure.lang.IFn [^clojure.lang.IFn$OOD model
                                         ^java.util.Map attributes]
  (let [predictors (dissoc attributes :ground-truth :prediction)
        ^clojure.lang.IFn$OD ground-truth (:ground-truth attributes)
        _ (assert (instance? clojure.lang.IFn$OD ground-truth))
        ^clojure.lang.IFn$OD yhat (fn yhat ^double [datum] 
                                    (.invokePrim model predictors datum))]
    (fn false-negative [datum] 
      (and (== 1.0 (.invokePrim ground-truth datum))
           (== 0.0 (.invokePrim yhat datum))))))
;;------------------------------------------------------------------------------
(defn true-positives 
  
  "A [metric function](1metrics.html).
   Returns the count of elements of <code>data</code> where both
   ground truth and model prediction are 1.0.
   <dl>
   <dt><code>model</code></dt>
   <dd>a <code>clojure.lang.IFn$OOD</code> that takes an attribute map and a
   datum, one of the elements of <code>data</code>.
   </dd>
   <dt><code>attributes</code></dt><dd>a <code>java.util.Map</code> from
   keywords to functions that can be applied to the elements of 
   <code>data</code>. Must contain <code>:ground-truth</code> as a key.
   </dd>
   <dt><code>data</code></dt><dd>an <code>Iterable</code></dd>
   </dl>"
  
  ^double [^clojure.lang.IFn$OOD model
           ^java.util.Map attributes
           ^Iterable data]
  (double (z/count (true-positive model attributes) data)))

(defn true-negatives 
  
  "A [metric function](1metrics.html).
   Returns the count of elements of <code>data</code> where both
   ground truth and model prediction are 0.0.
   <dl>
   <dt><code>model</code></dt>
   <dd>a <code>clojure.lang.IFn$OOD</code> that takes an attribute map and a
   datum, one of the elements of <code>data</code>.
   </dd>
   <dt><code>attributes</code></dt><dd>a <code>java.util.Map</code> from
   keywords to functions that can be applied to the elements of 
   <code>data</code>. Must contain <code>:ground-truth</code> as a key.
   </dd>
   <dt><code>data</code></dt><dd>an <code>Iterable</code></dd>
   </dl>"
  
  ^double [^clojure.lang.IFn$OOD model
           ^java.util.Map attributes
           ^Iterable data]
  (double (z/count (true-negative model attributes) data)))

(defn false-positives 
  
  "A [metric function](1metrics.html).
   Returns the count of elements of <code>data</code> where both
   ground truth is 0.0 and model prediction is 1.0.
   <dl>
   <dt><code>model</code></dt>
   <dd>a <code>clojure.lang.IFn$OOD</code> that takes an attribute map and a
   datum, one of the elements of <code>data</code>.
   </dd>
   <dt><code>attributes</code></dt><dd>a <code>java.util.Map</code> from
   keywords to functions that can be applied to the elements of 
   <code>data</code>. Must contain <code>:ground-truth</code> as a key.
   </dd>
   <dt><code>data</code></dt><dd>an <code>Iterable</code></dd>
   </dl>"
  
  ^double [^clojure.lang.IFn$OOD model
           ^java.util.Map attributes
           ^Iterable data]
  (double (z/count (false-positive model attributes) data)))

(defn false-negatives 
  
  "A [metric function](1metrics.html).
   Returns the count of elements of <code>data</code> where both
   ground truth is 1.0 and model prediction is 0.0.
   <dl>
   <dt><code>model</code></dt>
   <dd>a <code>clojure.lang.IFn$OOD</code> that takes an attribute map and a
   datum, one of the elements of <code>data</code>.
   </dd>
   <dt><code>attributes</code></dt><dd>a <code>java.util.Map</code> from
   keywords to functions that can be applied to the elements of 
   <code>data</code>. Must contain <code>:ground-truth</code> as a key.
   </dd>
   <dt><code>data</code></dt><dd>an <code>Iterable</code></dd>
   </dl>"
  
  ^double [^clojure.lang.IFn$OOD model
           ^java.util.Map attributes
           ^Iterable data]
  (double (z/count (false-negative model attributes) data)))
;;------------------------------------------------------------------------------
