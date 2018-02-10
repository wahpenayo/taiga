(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-02-09"
      :doc 
      "Affine (aka linear) models." }
    
    taiga.affine
  
  (:require [zana.api :as z])
  (:import [java.util Arrays]
           [clojure.lang IFn IFn$OD]
           [org.apache.commons.math3.stat.regression
            OLSMultipleLinearRegression]))
;;----------------------------------------------------------------
;; training options
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
;; generic training
;;----------------------------------------------------------------
(defn affine-l2-regression
  
  "Train an affine (aka linear) model by minimizing l2 prediction
   error on the training data.
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
  
  [options]
  
  #_(println (z/pprint-map-str options))
  (check options)
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
        ae (z/affine-embedding (str datum-name "->En") xs data)
        ;; commons math handles the intercept itself
        le (z/linear-part ae)
        lembed (fn ^doubles [datum] (le bindings datum))
        xembedded (z/map-to-objects (Class/forName "[D")
                                    lembed 
                                    data)
        ols (OLSMultipleLinearRegression.)
        _ (.newSampleData ols ys xembedded)
        ^doubles beta (.estimateRegressionParameters ols)
        ;; commons math has intercept as zeroth element
        ^IFn$OD af (z/affine-functional 
                     (z/linear-functional 
                       (Arrays/copyOfRange beta 1 (alength beta)))
                     (aget beta 0))]
    (println "beta:" (into [] beta))
    (println "af:" af)
    ;; NOTE: can't serialize this!!
    (fn predict ^double [bindings datum] 
      (.invokePrim af (ae bindings datum)))))
;;----------------------------------------------------------------
