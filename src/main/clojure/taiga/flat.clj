(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-14"
      :doc 
      "Flat (affine or linear) models.
      <p>
      Usual statistics 'linear' model is actually fitting an
      affine functional (with intercept case) or a linear 
      functional (no intercept case)." }
    
    taiga.flat
  
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
;; As it is, an FlatModel is an affine functional (a real-valued
;; function on <b>E</b><sup>p</sup>) composed with an affine
;; embedding of the relevant record type, a function from
;; the specified attributes of record objects to 
;; <b>E</b><sup>p</sup>.
;; Note: nothing here really requies the functional to be flat!
;; We are really just composing an embedding of data space into
;; a presumably flat space (though that could be more general)
;; with a 'functional', by which I just mean a real-valued 
;; function.

(deftype FlatModel [^IFn$OD functional
                    ^IFn embedding]
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
    (and (instance? FlatModel that)
         (.equals functional (.functional ^FlatModel that))
         (.equals embedding (.embedding ^FlatModel that))))
  (toString [_]
    (str "FlatModel[" functional ", " embedding "?}"))) 

(defn functional ^IFn$OD [^FlatModel am]
  (.functional am))
(defn embedding ^IFn [^FlatModel am]
  (.embedding am))
#_(defn functional ^AffineFunctional [^FlatModel am]
  (.functional am))
#_(defn embedding ^AffineEmbedding [^FlatModel am]
  (.embedding am))
;;----------------------------------------------------------------
;; EDN IO
;;----------------------------------------------------------------
(defn map->FlatModel ^FlatModel [^Map m] 
  (FlatModel. 
    ^IFn$OD (:functional m)
    ^IFn (:embedding m))
  #_(FlatModel. 
     ^AffineFunctional (:functional m)
     ^AffineEmbedding (:embedding m)))
(defn map<-FlatModel ^Map [^FlatModel this] 
  {:functional (.functional this) :embedding (.embedding this)})
(defmethod z/clojurize FlatModel [^FlatModel this]
  (map<-FlatModel this))
(defmethod print-method FlatModel [^FlatModel this ^Writer w]
  (if *print-readably*
    (do
      (.write w " #taiga.flat.FlatModel ")
      (.write w (pr-str (map<-FlatModel this))))
    (.write w (print-str (map<-FlatModel this)))))
;;----------------------------------------------------------------
;; EDN input (output just works?)
;;----------------------------------------------------------------
(z/add-edn-readers! 
  {'taiga.flat.FlatModel map->FlatModel})
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
  ;; edge case: zero predictotrs means a constant model
  ;; TODO: force constants to be different from affine?
  (assert (<= 0 (count (dissoc (:attributes options) 
                               :ground-truth :prediction)))
          (str ":no predictors:" options))
  (assert (every? ifn? (:attributes options)) 
          (str "One of the :attributes is not a function."
               (z/pprint-map-str (:attributes options))))
  (assert (ifn? (:ground-truth (:attributes options)))
          (str "No :ground-truth!"
               (z/pprint-map-str options))))
;;----------------------------------------------------------------
#_(defn- affinate
  
  "Convert the general prediction problem to affine form
   (aka linear with intercept)."
  
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
(defn- linearize
  
  "Convert the general prediction problem to pure (no intercept) 
   linear form."
  
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
        ;; need linear embedding for predictions later
        e (:embedding 
             options
             (z/linear-embedding (str datum-name "->En") xs data))]
    [ys e bindings data]))
;;----------------------------------------------------------------
;; with intercept models
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
  (let [[y le bindings data] (linearize options)
        embed (fn ^doubles [datum] (le bindings datum))
        x (z/map-to-objects (Class/forName "[D") embed data)
        ols (OLSMultipleLinearRegression.)
        _ (.newSampleData ols y x)
        ^doubles beta (.estimateRegressionParameters ols)
          ;; commons math has intercept as zeroth element
          ^IFn$OD af (z/affine-functional 
                       (z/linear-functional 
                         (Arrays/copyOfRange beta 1 (alength beta)))
                       (aget beta 0))]
    (FlatModel. af le)))
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
  (let [[y le bindings data] (linearize options)
        embed (fn ^doubles [datum] (le bindings datum))
        x (z/map embed data)
        l2d2 (z/l2distance2-from y)
        sample (z/sampler x)
        ;; dimension of linear homogeneous coordinate space
        n+1 (inc (.dimension ^LinearEmbedding le))
        start (double-array n+1 0.0)
        #_ (println "affine" (Arrays/toString start))
        adual (z/affine-dual (Dn/get n+1))
        costf (z/compose l2d2 (z/compose sample adual))
        #_(println "costf" costf)
        cg-options (merge {:objective costf
                           :start start
                           :max-iterations 1000}
                          options)
        [^doubles beta ^double cost] (z/optimize-cg cg-options)]
    (FlatModel. (z/affine-functional beta) le)))
;;----------------------------------------------------------------
(defn affine-l1
  
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
  (let [[y le bindings data] (linearize options)
        embed (fn ^doubles [datum] (le bindings datum))
        x (z/map embed data)
        hd (z/huberdistance-from 
              (:huber-epsilon options)
              y)
        sample (z/sampler x)
        ;; dimension of linear homogeneous coordinate space
        n+1 (inc (.dimension ^LinearEmbedding le))
        adual (z/affine-dual (Dn/get n+1))
        costf (z/compose hd (z/compose sample adual))
        #_(println "costf" (.toString ^Object costf))
;        costf (if (:trace options)
;                (z/trace-function costf (:trace options))
;                costf)
        costf (if (:gradient-check options)
                (z/gradient-check costf (:gradient-check options))
                costf)
        #_(println "costf" (.toString ^Object costf))
        cg-options (merge {:objective costf
                           :start (double-array n+1 0.0)
                           :max-iterations 100}
                          options)
        [^doubles beta ^double cost] (z/optimize-cg cg-options)]
    (FlatModel. (z/affine-functional beta) le)))
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
  (let [[y le bindings data] (linearize options)
        embed (fn ^doubles [datum] (le bindings datum))
        x (z/map embed data)
        qrd (z/qrdistance-from 
              (:quantile-p options)
              (:huber-epsilon options)
              y)
        sample (z/sampler x)
        ;; dimension of linear homogeneous coordinate space
        n+1 (inc (.dimension ^LinearEmbedding le))
        adual (z/affine-dual (Dn/get n+1))
        costf (z/compose qrd (z/compose sample adual))
        #_(println "costf" costf)
        cg-options (merge {:objective costf
                           :start (double-array n+1 0.0)
                           :max-iterations 100}
                          options)
        [^doubles beta ^double cost] (z/optimize-cg cg-options)]
    (FlatModel. (z/affine-functional beta) le)))
;;----------------------------------------------------------------
;; no intercept models
;;----------------------------------------------------------------
(defn linear-l2-regression
  
  "Train a  linear (no intercept) model by minimizing l2 prediction
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
  (let [[y le bindings data] (linearize options)
        embed (fn ^doubles [datum] (le bindings datum))
        x (z/map-to-objects (Class/forName "[D") embed data)
        ols (OLSMultipleLinearRegression.)
        _ (.setNoIntercept ols true)
        _ (.newSampleData ols y x)
        ^doubles beta (.estimateRegressionParameters ols)]
    (FlatModel. (z/linear-functional beta) le)))
;;----------------------------------------------------------------
(defn linear-l2
  
  "Train a  linear (no intercept) model by minimizing possibly
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
  (let [[y le bindings data] (linearize options)
        embed (fn ^doubles [datum] (le bindings datum))
        x (z/map embed data)
        l2d2 (z/l2distance2-from y)
        sample (z/sampler x)
        n (.dimension ^LinearEmbedding le)
        start (double-array n 0.0)
        #_ (println "affine" (Arrays/toString start))
        ldual (z/linear-dual (Dn/get n))
        costf (z/compose l2d2 (z/compose sample ldual))
        #_(println "costf" costf)
        cg-options (merge {:objective costf
                           :start start
                           :max-iterations 1000}
                          options)
        [^doubles beta ^double cost] (z/optimize-cg cg-options)]
    (FlatModel. (z/linear-functional beta) le)))
;;----------------------------------------------------------------
(defn linear-l1
  
  "Train a  linear (no intercept) model by minimizing possibly
   regularized huberized l1 prediction cost on 
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
  (let [[y le bindings data] (linearize options)
        embed (fn ^doubles [datum] (le bindings datum))
        x (z/map embed data)
        hd (z/huberdistance-from 
              (:huber-epsilon options)
              y)
        sample (z/sampler x)
        ;; dimension of linear homogeneous coordinate space
        n (.dimension ^LinearEmbedding le)
        ldual (z/linear-dual (Dn/get n))
        costf (z/compose hd (z/compose sample ldual))
        #_(println "costf" (.toString ^Object costf))
;        costf (if (:trace options)
;                (z/trace-function costf (:trace options))
;                costf)
        costf (if (:gradient-check options)
                (z/gradient-check costf (:gradient-check options))
                costf)
        #_(println "costf" (.toString ^Object costf))
        cg-options (merge {:objective costf
                           :start (double-array n 0.0)
                           :max-iterations 100}
                          options)
        [^doubles beta ^double cost] (z/optimize-cg cg-options)]
    (FlatModel. (z/linear-functional beta) le)))
;;----------------------------------------------------------------
(defn linear-qr
  
  "Train a  linear (no intercept) model by minimizing possibly
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
  (let [[y le bindings data] (linearize options)
        embed (fn ^doubles [datum] (le bindings datum))
        x (z/map embed data)
        qrd (z/qrdistance-from 
              (:quantile-p options)
              (:huber-epsilon options)
              y)
        sample (z/sampler x)
        ;; dimension of linear homogeneous coordinate space
        n (.dimension ^LinearEmbedding le)
        ldual (z/linear-dual (Dn/get n))
        costf (z/compose qrd (z/compose sample ldual))
        #_(println "costf" costf)
        cg-options (merge {:objective costf
                           :start (double-array n 0.0)
                           :max-iterations 100}
                          options)
        [^doubles beta ^double cost] (z/optimize-cg cg-options)]
    (FlatModel. (z/linear-functional beta) le)))
;;----------------------------------------------------------------
