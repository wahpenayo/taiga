(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-02"
      :doc 
      "Affine (aka linear) models." }
    
    taiga.affine
  
  (:refer-clojure :exclude [flatten])
  (:require [zana.api :as z])
  (:import [java.util Arrays Map]
           [java.io Writer]
           [clojure.lang IFn IFn$OD IFn$OOD]
           [org.apache.commons.math3.stat.regression
            OLSMultipleLinearRegression]
           [zana.java.geometry Dn]
           #_[zana.java.geometry.functions AffineFunctional]
           [zana.java.data AffineEmbedding LinearEmbedding]))
;;----------------------------------------------------------------
;; This ought to be just a function composition, but we need to 
;; define a special class so it can be serialized.
;; As it is, an AffineModel is an affine functional (a real-valued
;; function on <b>E</b><sup>p</sup>) composed with an affine
;; embedding of the relevant record type, a function from
;; the specified attributes of record objects to 
;; <b>E</b><sup>p</sup>.
;; TODO: support linear as well as affine, ie, general to flat
;; functionals and embeddings?

(deftype AffineModel [^IFn$OD functional
                      ^IFn embedding]
  #_[^AffineFunctional functional
     ^AffineEmbedding embedding]
  java.io.Serializable
  IFn$OOD 
  (invokePrim ^double [_ bindings record]
    (.invokePrim ^IFn$OD functional
      (.invoke ^IFn embedding bindings record)))
  IFn 
  (invoke [this bindings record]
    (.invokePrim ^IFn$OOD this bindings record))
  Object
  (hashCode [_]
    (let [h (int 17)
          h (unchecked-multiply-int h (int 31))
          h (unchecked-add-int h (.hashCode functional))
          h (unchecked-multiply-int h (int 31))
          h (unchecked-add-int h (.hashCode embedding))]
      h))
  (equals [_ that]
    (and (instance? AffineModel that)
         (.equals functional (.functional ^AffineModel that))
         (.equals embedding (.embedding ^AffineModel that))))
  (toString [_]
    (str "AffineModel[" functional ", " embedding "?}"))) 

(defn functional ^IFn$OD [^AffineModel am]
  (.functional am))
(defn embedding ^IFn [^AffineModel am]
  (.embedding am))
#_(defn functional ^AffineFunctional [^AffineModel am]
  (.functional am))
#_(defn embedding ^AffineEmbedding [^AffineModel am]
  (.embedding am))
;;----------------------------------------------------------------
;; EDN IO
;;----------------------------------------------------------------
(defn map->AffineModel ^AffineModel [^Map m] 
  (AffineModel. 
    ^IFn$OD (:functional m)
    ^IFn (:embedding m))
  #_(AffineModel. 
     ^AffineFunctional (:functional m)
     ^AffineEmbedding (:embedding m)))
(defn map<-AffineModel ^Map [^AffineModel this] 
  {:functional (.functional this) :embedding (.embedding this)})
(defmethod z/clojurize AffineModel [^AffineModel this]
  (map<-AffineModel this))
(defmethod print-method AffineModel [^AffineModel this ^Writer w]
  (if *print-readably*
    (do
      (.write w " #taiga.affine.AffineModel ")
      (.write w (pr-str (map<-AffineModel this))))
    (.write w (print-str (map<-AffineModel this)))))
;;----------------------------------------------------------------
;; EDN input (output just works?)
;;----------------------------------------------------------------
(z/add-edn-readers! 
  {'taiga.affine.AffineModel map->AffineModel})
;;----------------------------------------------------------------
;; training
;;----------------------------------------------------------------
(defn- check [options]
  (assert (not (z/empty? (:data options)))
          (print-str "no :data in" 
                     (z/pprint-map-str options)))
  (assert (:attributes options) 
          (str "No :attributes given." 
               (z/pprint-map-str options)))
  (assert (instance? java.util.Map (:attributes options)) 
          (str ":attributes is not a map."
               (z/pprint-map-str options)))
  (assert (< 0 (count (dissoc (:attributes options) 
                              :ground-truth :prediction)))
          (str ":no predictors:" options))
  (assert (every? ifn? (:attributes options)) 
          (str "One of the :attributes is not a function."
               (z/pprint-map-str (:attributes options))))
  (assert (ifn? (:ground-truth (:attributes options)))
          (str "No :ground-truth!"
               (z/pprint-map-str options))))
;;----------------------------------------------------------------
(defn- flatten
  
  "Convert the general prediction problem to affine form."
  
  [options]
  
  (let [^IFn$OD y (:ground-truth (:attributes options))
        data (z/drop-missing y (:data options))
        _ (assert (not (z/empty? data)))
        datum-name (.getSimpleName (class (z/first data)))
        ^doubles ys (z/map-to-doubles y data)
        ;; TODO: can we be sure the order of bindings and
        ;; (vals bindings) would be the same?
        bindings (into (sorted-map)
                       (dissoc (:attributes options) 
                               :ground-truth 
                               :prediction))
        xs (vals bindings)
        ;; need affine embedding for predictions later
        ae (:embedding 
             options
             (z/affine-embedding (str datum-name "->En") xs data))]
    [ys ae bindings data]))
;;----------------------------------------------------------------
(defn affine-l2-regression
  
  "Train an affine (aka linear) model by minimizing l2 prediction
   error on the training data, using apache commons math3
   <a href=\"http://commons.apache.org/proper/commons-math/javadocs/api-3.6.1/org/apache/commons/math3/stat/regression/OLSMultipleLinearRegression.html\">
   <code>OLSMultipleLinearRegression</code></a> to do the fitting
   <p>   
   <code>^clojure.lang.IPersistentMap options</code></dt>
   a map containing options used directly by 
   <code>l2</code> and passed to functions called by 
   <code>l2</code>:
   <dl>
   <dt><code>:attributes</code></dt>
   <dd> 
   a map from keywords to attribute functions, functions which can
   be applied to the elements of <code>:data</code>.
   Must include <code>:ground-truth</code>. Key-value pairs other 
   than <code>:ground-truth</code> and <code>:prediction</code> 
   are used as predictors in the forest.<br>
   Attribute functions are treated as numerical if they implement 
   <code>clojure.lang.IFn$OD</code> or if their value on every 
   element of <code>:data</code> is a <code>Number</code>. 
   Implementing <code>clojure.lang.IFn$OD</code> will result in 
   much higher performance.
   <br>
   Missing data for numerical attributes is represented by 
   returning <code>Double/NaN</code>.
   <br>
   Any other attribute functions are treated as categorical. There
   is no hard limit on the number of categories allowed. Too large 
   a number of categories is likely to result in overfitting. The 
   best way to detect overfitting is to compare accuracy on the 
   training data to accuracy on a fair test set. If training 
   accuracy is much better, looking at the number of distinct 
   values for the categorical attributes is one of the first 
   things to check.
   </dd>
   <dt><code>:data</code></dt>
   <dd>
   a finite <code>Iterable</code> whose elements are valid inputs 
   to the attribute functions.
   </dd>
   <dt><code>:embedding</code></dt>
   <dd>
   TODO
   </dd>
   </dl>"
  
  ^IFn$OOD [options]
  
  #_(println (z/pprint-map-str options))
  (check options)
  (let [[y ae bindings data] (flatten options)
        ;; need corresponding linear embedding for commons math,
        ;; which handles the intercept itself
        le (z/linear-embedding ae)
        embed (fn ^doubles [datum] (le bindings datum))
        x (z/map-to-objects (Class/forName "[D") embed data)
        ols (OLSMultipleLinearRegression.)
        _ (.newSampleData ols y x)
        ^doubles beta (.estimateRegressionParameters ols)
        #_ (println "beta:" (into [] beta))
          ;; commons math has intercept as zeroth element
          ^IFn$OD af (z/affine-functional 
                       (z/linear-functional 
                         (Arrays/copyOfRange beta 1 (alength beta)))
                       (aget beta 0))]
    (AffineModel. af ae)))
;;----------------------------------------------------------------
(defn affine-l2
  
  "Train an affine (aka linear) model by minimizing possibly 
   regularized l2 prediction
   error on the training data, using nonlinear conjugate gradients
   (<code>zana/optimize-cg</code>) to do the fitting. 
   <p>
   <code>^clojure.lang.IPersistentMap options</code></dt>
   a map containing options used directly by 
   <code>l2</code> and passed to functions called by 
   <code>l2</code>:
   <dl>
   <dt><code>:attributes</code></dt>
   <dd> 
   a map from keywords to attribute functions, functions which can
   be applied to the elements of <code>:data</code>.
   Must include <code>:ground-truth</code>. Key-value pairs other 
   than <code>:ground-truth</code> and <code>:prediction</code> 
   are used as predictors in the forest.<br>
   Attribute functions are treated as numerical if they implement 
   <code>clojure.lang.IFn$OD</code> or if their value on every 
   element of <code>:data</code> is a <code>Number</code>. 
   Implementing <code>clojure.lang.IFn$OD</code> will result in 
   much higher performance.
   <br>
   Missing data for numerical attributes is represented by 
   returning <code>Double/NaN</code>.
   <br>
   Any other attribute functions are treated as categorical. There
   is no hard limit on the number of categories allowed. Too large 
   a number of categories is likely to result in overfitting. The 
   best way to detect overfitting is to compare accuracy on the 
   training data to accuracy on a fair test set. If training 
   accuracy is much better, looking at the number of distinct 
   values for the categorical attributes is one of the first 
   things to check.
   </dd>
   <dt><code>:data</code></dt>
   <dd>
   a finite <code>Iterable</code> whose elements are valid inputs 
   to the attribute functions.
   </dd>
   <dt><code>:embedding</code></dt>
   <dd>
   TODO
   </dd>
   </dl>"
  
  ^IFn$OOD [options]
  
  #_(println (z/pprint-map-str options))
  (check options)
  ;; TODO: straighten out affine vs linear embeddings
  (let [[y ae bindings data] (flatten options)
        le (z/linear-embedding ae)
        embed (fn ^doubles [datum] (le bindings datum))
        x (z/map embed data)
        l2d2 (z/l2distance2-from y)
        sample (z/sampler x)
        ;; dimension of linear homogeneous coordinate space
        n+1 (inc (.dimension ^LinearEmbedding le))
        start (double-array n+1 0.0)
        _ (println "affine" (Arrays/toString start))
        adual (z/affine-dual (Dn/get n+1))
        costf (z/compose l2d2 (z/compose sample adual))
        _(println "costf" costf)
        cg-options (merge {:objective costf
                           :start start
                           :max-iterations 1000}
                          options)
        [^doubles beta ^double cost] (z/optimize-cg cg-options)]
    (AffineModel. (z/affine-functional beta) ae)))
;;----------------------------------------------------------------
(defn affine-qr
  
  "Train an affine (aka linear) model by minimizing possibly 
   regularized huberized quantile regression prediction cost on 
   the training data, using nonlinear conjugate gradients
   (<code>zana/optimize-cg</code>) to do the fitting. 
   <p>
   <code>^clojure.lang.IPersistentMap options</code></dt>
   a map containing options used directly by 
   <code>l2</code> and passed to functions called by 
   <code>l2</code>:
   <dl>
   <dt><code>:attributes</code></dt>
   <dd> 
   a map from keywords to attribute functions, functions which can
   be applied to the elements of <code>:data</code>.
   Must include <code>:ground-truth</code>. Key-value pairs other 
   than <code>:ground-truth</code> and <code>:prediction</code> 
   are used as predictors in the forest.<br>
   Attribute functions are treated as numerical if they implement 
   <code>clojure.lang.IFn$OD</code> or if their value on every 
   element of <code>:data</code> is a <code>Number</code>. 
   Implementing <code>clojure.lang.IFn$OD</code> will result in 
   much higher performance.
   <br>
   Missing data for numerical attributes is represented by 
   returning <code>Double/NaN</code>.
   <br>
   Any other attribute functions are treated as categorical. There
   is no hard limit on the number of categories allowed. Too large 
   a number of categories is likely to result in overfitting. The 
   best way to detect overfitting is to compare accuracy on the 
   training data to accuracy on a fair test set. If training 
   accuracy is much better, looking at the number of distinct 
   values for the categorical attributes is one of the first 
   things to check.
   </dd>
   <dt><code>:data</code></dt>
   <dd>
   a finite <code>Iterable</code> whose elements are valid inputs 
   to the attribute functions.
   </dd>
   <dt><code>:embedding</code></dt>
   <dd>
   TODO
   </dd>
   </dl>"
  
  ^IFn$OOD [options]
  
  #_(println (z/pprint-map-str options))
  (check options)
  ;; TODO: straighten out affine vs linear embeddings
  (let [[y ae bindings data] (flatten options)
        le (z/linear-embedding ae)
        embed (fn ^doubles [datum] (le bindings datum))
        x (z/map embed data)
        l2d2 (z/qrdistance-from 
               (:p options)
               (:epsilon options)
               y)
        sample (z/sampler x)
        ;; dimension of linear homogeneous coordinate space
        n+1 (inc (.dimension ^LinearEmbedding le))
        start (double-array n+1 0.0)
        _ (println "affine" (Arrays/toString start))
        adual (z/affine-dual (Dn/get n+1))
        costf (z/compose l2d2 (z/compose sample adual))
        _(println "costf" costf)
        cg-options (merge {:objective costf
                           :start start
                           :max-iterations 100}
                          options)
        [^doubles beta ^double cost] (z/optimize-cg cg-options)]
    (AffineModel. (z/affine-functional beta) ae)))
;;----------------------------------------------------------------
