(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2016-11-09"
      :doc "Importance, etc., via permutation." }
    
    taiga.permutation
  
  (:require [clojure.string :as s]
            [zana.api :as z]))
;;------------------------------------------------------------------------------
;; TODO: move permutation statistics to Zana?
;; probably would need a different api --- 
;;------------------------------------------------------------------------------
;; Call (prng) to get a generator with the same seed each time, 
;; for reproducibility.
(defn- mersenne-twister [] 
  (z/mersenne-twister-generator "D5B93C275D55B6871F5F72FD812B5572"))
;;------------------------------------------------------------------------------
(defn- permute ^java.util.Map [^clojure.lang.Keyword k 
                               ^java.util.Map attributes
                               ^Iterable data
                               ^java.util.Random prng]
  (if k
    (assoc attributes 
           k 
           (z/lookup-function (.get attributes k) data (z/shuffle data prng)))
    attributes))
;;------------------------------------------------------------------------------
(defn- metrics-record ^java.util.Map [^java.util.Map metrics
                                      ^clojure.lang.IFn$OOD model
                                      ^java.util.Map attributes
                                      ^Iterable data]
  (into (sorted-map)
        (z/map (fn [_ ^clojure.lang.IFn$OOOD metric]
                 (metric model attributes data))
               metrics)))
;;------------------------------------------------------------------------------
(defn permutation-statistics
  
  "Compute the values of each metric on the raw data, and with each attribute's
   values randomly permuted.
   <dl>
   <dt><code>^java.util.Map metrics</code></dt>
   <dd> A map from keyword (metric name) to [metric function](1metrics.html).
   </dd>
   <dt><code>^clojure.lang.IFn$OOD model</code></dt>
   <dd> A function that takes an attribute map and a single datum, and returns
          a double-valued prediction.
   </dd>
   <dt><code>^java.util.Map attributes</code></dt>
   <dd> A map from keyword (attribute name) to a function that takes a
        single datum and returns a double attribute value. Must include a value
        for <code>:ground-truth</code>.
   <dt><code>^Iterable data</code></dt>
   </dd>
   <dd> an Iterable over a set of training or test examples for which the 
         ground truth is known.
   </dd>
   <dt><code>^java.util.Random prng</code></dt>
   <dd> Pseudo-random number generator used for generating permutations.
   Defaults to a 
   [mersenne twister](http://maths.uncommons.org/api/org/uncommons/maths/random/MersenneTwisterRNG.html) 
   from [Uncommons Maths](http://maths.uncommons.org/).
   </dd>
   </dl>"
  
  (^java.util.Map [^java.util.Map metrics
                   ^clojure.lang.IFn$OOD model
                   ^java.util.Map attributes
                   ^Iterable data 
                   ^java.util.Random prng]
    
    (assert (:ground-truth attributes) (print-str (sort (keys attributes))))
    (let [predictors (dissoc attributes :ground-truth :prediction)
          ground-truth (:ground-truth attributes)]
      
      (assert (instance? clojure.lang.IFn$OD ground-truth))
      
      (into (sorted-map)
            (z/pmap (fn [[k ^clojure.lang.IFn$OD x]]
                      (let [px (permute k attributes data prng)]
                        [k (metrics-record metrics model px data)]))
                    (sort-by #(z/name (key %)) (assoc predictors nil nil))))))
  
  (^java.util.Map [^java.util.Map metrics
                   ^clojure.lang.IFn$OOD model
                   ^java.util.Map attributes
                   ^Iterable data]
    (permutation-statistics metrics model attributes data (mersenne-twister))))
;;------------------------------------------------------------------------------

(defn statistics-tsv 
  
  "**TODO:** move this to a generic tsv writer in Zana
   <dl>
   <dt><code>^java.util.Map stats</code></dt>
   <dd> a nested map. outer keys are predictor names, inner keys are metric 
        names. nil outer key has the results with no permutation.
   </dd>
   <dt><code>^java.io.File file</code>/dt>
   <dd> where to write the tab separated record, with the header gotten by
        stripping \":\" from the Keyword keys.
   </dd>
   </dl>"
  
  [^java.util.Map stats
   ^java.io.File file]
  (let [[predictor record] (first stats)
        header (keys record)]
    (with-open [w (z/print-writer file)]
      (.println w (s/join "\t" (cons "predictor" (mapv z/name header))))
      (doseq [[predictor record] (sort-by key stats)]
        (.println w 
          (s/join "\t" (cons (z/name predictor) 
                             (mapv #(get record %) header))))))))
;;------------------------------------------------------------------------------

