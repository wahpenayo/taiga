(set! *warn-on-reflection* true)
(set! *unchecked-math* false) ;; cheshire has boxed math warnings.
(ns ^{:author ["wahpenayo at gmail dot com"
               "John Alan McDonald" 
               "Kristina Lisa Klinkner"] 
      :date "2017-01-04"
      :doc "Random and other forests." }
    
    taiga.forest
  
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [cheshire.core :as cheshire]
            [zana.api :as z]
            [taiga.bagging :as bag]
            [taiga.ensemble :as ensemble]
            [taiga.tree.node :as node]
            [taiga.split.api :as split]
            [taiga.tree.leaf.double :as double-leaf]
            [taiga.tree.leaf.doubles :as doubles-leaf]
            [taiga.tree.grow :as grow]))
(set! *unchecked-math* :warn-on-boxed)
;;------------------------------------------------------------------------------
;; training options
;;------------------------------------------------------------------------------
(defn- classification-mtry ^long [options]
  (let [predictors (dissoc (:attributes options) :ground-truth :prediction)]
    (long (:mtry options (Math/floor (Math/sqrt (z/count predictors)))))))
;;------------------------------------------------------------------------------
(defn- regression-mtry ^long [options]
  (let [predictors (dissoc (:attributes options) :ground-truth :prediction)]
    (long (:mtry options (Math/floor (/ (z/count predictors) 3.0))))))
;;------------------------------------------------------------------------------
(defn- classification-mincount ^long [options] (long (:mincount options 5)))
(defn- regression-mincount ^long [options] (long (:mincount options 5)))
;;------------------------------------------------------------------------------
(defn- mincount-tester [options]
  (let [mincount (long (:mincount options))]
    (fn mincount-split? 
      ([a b] (split/mincount-split? mincount a b))
      ([a] (split/mincount-split? mincount a)))))
;;------------------------------------------------------------------------------
(defn- weighted? [options]
  (and (:weight options) (not= z/constantly-1d (:weight options))))
;;------------------------------------------------------------------------------
(defn- check [options]
  (assert (not (z/empty? (:data options)))
          (print-str "no :data in" (z/pprint-map-str options)))
  (assert (:attributes options) 
          (str "No :attributes given." (z/pprint-map-str options)))
  (assert (instance? java.util.Map (:attributes options)) 
          (str ":attributes is not a map." (z/pprint-map-str options)))
  (assert (< 0 (count (dissoc (:attributes options) :ground-truth :prediction)))
          (str ":no predictors:" options))
  (assert (every? ifn? (:attributes options)) 
          (str "One of the :attributes is not a function."
               (z/pprint-map-str (:attributes options))))
  (assert (ifn? (:ground-truth (:attributes options)))
          (str "No :ground-truth!"(z/pprint-map-str options)))
  (assert (< 0 (int (:nterms options))) 
          (str ":nterms must be positive:"(z/pprint-map-str options)))
  (assert (< 0 (int (:mincount options))) 
          (str ":mincount must be positive:"(z/pprint-map-str options)))
  (assert (< 0 (int (:maxdepth options)))
          (str ":maxdepth must be positive:"(z/pprint-map-str options)))
  (assert (< 0 (int (:mtry options))) 
          (str ":mtry must be positive:"(z/pprint-map-str options))))
;;------------------------------------------------------------------------------
;; generic training
;;------------------------------------------------------------------------------
(defn- random-forest-splitter [options prng]
  (assert (integer? (:mtry options)))
  (let [mtry (long (:mtry options))]
    (fn rf-splitter ^taiga.tree.node.Node [predictors data]
      (when-not (z/empty? predictors)
        (let [predictors (z/sample prng mtry predictors)]
          (split/best-split (assoc options 
                                   :predictors predictors 
                                   :data data)))))))
;;------------------------------------------------------------------------------
(defn random-forest 
  
  "Train a random forest.
   <dl>
   <dt><code>^clojure.lang.IPersistentMap options</code></dt>
   <dd>
   a map containing options used directly by <code>random-forest</code>
   and passed to functions called by <code>random-forest</code>, which allows
   the caller to pass custom options to, for example, their custom
   <code>:cost-factory</code>.<br>
   Note that <code>:cost-factory</code>, <code>:leaf-learner</code>, and 
   <code>:score</code> need to be coordinated for the results to make sense.
   <dl>
   <dt><code>:attributes</code></dt>
   <dd> 
   a map from keywords to attribute functions, functions which can be 
   applied to the elements of <code>:data</code>.
   Must include <code>:ground-truth</code>. Key-value pairs other than
   <code>:ground-truth</code> and <code>:prediction</code> are used as
   predictors in the forest.<br>
   Attribute functions are treated as numerical if they implement 
   <code>clojure.lang.IFn$OD</code> or if their value on every element of 
   <code>:data</code> is a <code>Number</code>. 
   Implementing 
   <code>clojure.lang.IFn$OD</code> will result in much higher performance.
   <br>
   Missing data for numerical attributes is represented by returning 
   <code>Double/NaN</code>.
   <br>
   Any other attribute functions are treated as categorical. There is no hard 
   limit on the number of categories allowed. Too large a number of categories
   is likely to result in overfitting. The best way to detect overfitting is
   to compare accuracy on the training data to accuracy on a fair test set. If
   training accuracy is much better, looking at the number of distinct values
   for the categorical attributes is one of the first things to check.
   </dd>
   <dt><code>:combine</code></dt>
   <dd>
   A factory function for building models from term collections.
   An (inefficient) example:
   <pre>
   <code>
   (defn mean-combiner [terms]
     (fn [datum] (mean (map (fn [term] (term datum)) terms))))
   </code>
   </pre>
   </dd>
   <dt><code>:cost-factory</code></dt>
   <dd>  
   A factory function that returns an instance of 
   <code>zana.java.accumulator.Accumulator</code>.
   Accumulators are mutable objects that maintain the value of some statistics
   as data is added and removed.<br>
   Like most decision tree implementations, Taiga does brute force split 
   optimization, iterating over every possible split point on each attribute.
   This is much faster when the cost of a split can be updated by moving cases
   from one side of the split to the other (as opposed to recalculating the cost
   from scratch for the 2 new subsets of the training data). 
   </dd>
   <dt><code>:data</code></dt>
   <dd>
   a finite <code>Iterable</code> whose elements are valid inputs to the 
   attribute functions.
   </dd>
   <dt><code>:feasible?</code></dt>
   <dd>
   a predicate function used to exclude unacceptable splits from
   consideration. Its arguments are 1 or 2 Accumulators:<br>
   <code>(feasible? cost)</code>.<br>
   <code>(feasible? left-cost right-cost)</code>.<br>
   The most common case is a check for a minimum number of training cases in
   a node, or in both the left and right sides of the split, 
   using <code>:mincount</code>.
   </dd>
   <dt><code>:leaf-learner</code></dt>
   <dd>
   A function that maps a data collection to a Leaf node, which is also the
   function used to compute the prediction for any datum that ends up in that 
   Leaf. For example, 
   in L2 regression, the leaf learner calculates the mean of the ground truth
   over the training cases in the node. The Leaf is then just a constant
   function that returns that mean.
   </dd>
   <dt><code>:mincount</code></dt>
   <dd> 
   The minimum number of training cases in any node --- no split will be 
   accepted if it has fewer than <code>:mincount</code> training cases on either
   side.<br>
   Currently required, but to be deprecated. 
   This overlaps with <code>:feasible?</code> and should be absorbed into the 
   split feasibility function definition.
   </dd>
   <dt><code>:mtry</code></dt>
   <dd> 
   When choosing a split, choose the best attribute from a random subsample
   of the attributes of size <code>:mtry</code>.
   </dd>
   <dt><code>:nterms</code></dt>
   <dd> 
   How many trees in the forest?
   </dd>
   <dt><code>:score</code></dt>
   <dd> 
   Currently, <code>:score</code> is a function that takes an array of 
   <code>double</code> and returns a double. It's used to sort categories as
   part of optimizing a split on a categorical attribute. In general, one would
   need to consider all possible partitions of the categories into 2 subsets.
   In special cases (L2 regression and binary classification). it can be shown 
   that the optimal split can be found by scoring each category by applying the
   right score function to the cases that have that category, sorting on the
   category scores, and picking the best left/right split of the sorted 
   categories.
   </dd>
   <dt><code>:nthreads</code> Optional. Defaults to 
   <code>(.availableProcessors (Runtime/getRuntime))</code>)</dt>
   <dd> 
   How many trees to train concurently? 
   Using the default value is a good start, but you may want a lower value
   if your cpu has other tasks, or if you observe the training creating 
   garbage faster than the JVM can collect it.
   </dd>
   <dt><code>:weight</code> Optional. Defaults to a constant 1.0.</dt>
   <dd> 
   A double-valued function that returns and importance weight for 
   each datum. Some cost functions, scores and leaf learners use it, some don't.
   </dd>
   </dl>
   </dd>
   </dl>"
  
  [options]
  
  #_(println (z/pprint-map-str options))
  (check options)
  (let [y (:ground-truth (:attributes options))
        data (z/drop-missing y (:data options))
        predictors (dissoc (:attributes options) :ground-truth :prediction)
        options (assoc options
                       :data data
                       :ground-truth y
                       :predictors predictors)
        bagging-prng (z/mersenne-twister-generator)
        split-prngs (doall (repeatedly (:nterms options) 
                                       z/mersenne-twister-generator))
        splitters (z/map #(random-forest-splitter options %) split-prngs)
        learners (z/map 
                   #(fn learn [data] 
                      (grow/learn 
                        (:maxdepth options Integer/MAX_VALUE)
                        (:leaf-learner options) 
                        % 
                        predictors 
                        data))
                   splitters)]
    (bag/bagging options bagging-prng learners)))
;;------------------------------------------------------------------------------
;; (scalar) regression
;;------------------------------------------------------------------------------
(defn mean-regression-options [options]
  "Fill in options map with standard defaults for simple L2 regression."
  (assoc 
    options
    :maxdepth (or (:maxdepth options) Integer/MAX_VALUE)
    :mincount (regression-mincount options)
    :mtry (regression-mtry options)
    :leaf-learner (double-leaf/mean-learner 
                    (:ground-truth (:attributes options)) (:weight options))
    :cost-factory (if (weighted? options)
                    z/weighted-mssn-accumulator
                    z/mssn-accumulator)
    :feasible? (mincount-tester options)
    :score (if (weighted? options)
             (z/make-calculator z/weighted-mean-accumulator)
             (z/make-calculator z/mean-accumulator))
    :combine ensemble/mean-model))
;;------------------------------------------------------------------------------
(defn mean-regression 
  
  "Train a standard (Breiman) regression random forest, optimizing for L2 cost 
   in numerical predictions.
   <dl>
   <dt><code>^clojure.lang.IPersistentMap options</code></dt>
   <dd>
   See [[random-forest]].<br>
   Overrides <code>:combine</code>, <code>:cost-factory</code>, 
   <code>:feasible</code>, <code>:leaf-learner</code>, and
   <code>:score</code> with the right choices for L2 regression (so the values
   in <code>options</code> will be silently ignored.
   Provides defaults for <code>:mincount</code> and <code>:mtry</code>.
   </dd>
   </dl>"
  
  ^taiga.ensemble.MeanModel [options]
  (random-forest (mean-regression-options options)))
;;------------------------------------------------------------------------------
;; vector-valued regression
;;------------------------------------------------------------------------------
;; TODO: better way to get dimension of output vector space ---
;; need dimension of codomain of ground-truth

(defn mean-vector-regression-options [options]
  "Fill in options map with standard defaults for vector-valued L2 regression."
  (assert (not (:weight options))
          "no supported for weighted vector-valued regression yet.")
  (assert (integer? (:codimension options)))
  (assoc 
    options
    :maxdepth (or (:maxdepth options) Integer/MAX_VALUE)
    :mincount (regression-mincount options)
    :mtry (regression-mtry options)
    :leaf-learner (doubles-leaf/mean-learner (:ground-truth (:attributes options))
                                             (:codimension options))
    :cost-factory #(z/vector-mssn-accumulator (:codimension options))
    :feasible? (mincount-tester options)
    :combine #(ensemble/mean-vector-model (:codimension options) %)))
;;------------------------------------------------------------------------------
(defn mean-vector-regression 
  
  "Train a standard (Breiman) regression random forest, optimizing for L2 cost 
   in numerical predictions.
   <dl>
   <dt><code>^clojure.lang.IPersistentMap options</code></dt>
   <dd>
   See [[random-forest]].<br>
   Overrides <code>:combine</code>, <code>:cost-factory</code>, 
   <code>:feasible</code>, <code>:leaf-learner</code>, and
   <code>:score</code> with the right choices for L2 regression (so the values
   in <code>options</code> will be silently ignored.
   Provides defaults for <code>:mincount</code> and <code>:mtry</code>.
   </dd>
   </dl>"
  
  ^taiga.ensemble.MeanModel [options]
  (random-forest (mean-vector-regression-options options)))
;;------------------------------------------------------------------------------
;; classification
;;------------------------------------------------------------------------------
(defn binary-classification-options [options]
  (assoc options
         :maxdepth (or (:maxdepth options) Integer/MAX_VALUE)
         :mincount (classification-mincount options)
         :mtry (classification-mtry options)
         :leaf-learner (double-leaf/majority-vote-learner 
                         (:ground-truth (:attributes options)) (:weight options))
         :cost-factory (if (weighted? options)
                         z/weighted-gini-accumulator 
                         z/gini-accumulator)
         :feasible? (mincount-tester options)
         :score (if (weighted? options)
                  (z/make-calculator z/weighted-positive-fraction-accumulator)
                  (z/make-calculator z/positive-fraction-accumulator))))
;;------------------------------------------------------------------------------
(defn majority-vote-classifier
  
  "Train a standard (Breiman) binary classification random forest, using the 
   gini cost to choose splits and majority vote as the leaf model and overall
   tree combiner. Uses case <code>:weight</code> if provided.
   <dl>
   <dt><code>^clojure.lang.IPersistentMap options</code></dt>
   <dd>
   See [[random-forest]].<br>
   Overrides <code>:combine</code>, <code>:cost-factory</code>, 
   <code>:feasible</code>, <code>:leaf-learner</code>, and
   <code>:score</code> with the right choices for L2 regression (so the values
   in <code>options</code> will be silently ignored.
   Provides defaults for <code>:mincount</code> and <code>:mtry</code>.
   </dd>
   </dl>"
  
  ^taiga.ensemble.MinimumCostClassModel [options]
  
  (random-forest (assoc (binary-classification-options options)
                        :combine ensemble/majority-model)))
;;------------------------------------------------------------------------------
(defn majority-vote-probability 
  
  "Train a standard (Breiman) binary classification random forest, using the 
   gini cost to choose splits and majority vote as the leaf model, but output
   the fraction of positive trees as an estimate of the probability of the
   positive class. Uses case <code>:weight</code> if provided.
   <dl>
   <dt><code>^clojure.lang.IPersistentMap options</code></dt>
   <dd>
   See [[random-forest]].<br>
   Overrides <code>:combine</code>, <code>:cost-factory</code>, 
   <code>:feasible</code>, <code>:leaf-learner</code>, and
   <code>:score</code> with the right choices for L2 regression (so the values
   in <code>options</code> will be silently ignored.
   Provides defaults for <code>:mincount</code> and <code>:mtry</code>.
   </dd>
   </dl>"
  
  ^taiga.ensemble.PositiveFractionModel [options]
  (random-forest (assoc (binary-classification-options options)
                        :combine ensemble/positive-fraction-model)))
;;------------------------------------------------------------------------------
(defn positive-fraction-probability-options [options]
  (assoc (binary-classification-options options)
         :leaf-learner (double-leaf/positive-fraction-learner 
                         (:ground-truth (:attributes options)) (:weight options))
         :combine ensemble/mean-model))
;;------------------------------------------------------------------------------
(defn positive-fraction-probability 
  
  "Train a standard (Breiman) binary classification random forest, using the 
   gini cost to choose splits. Use the fraction of positive cases as the leaf 
   model, and output the mean of the tree fractions as an estimate of the 
   probability of the positive class. Uses case <code>:weight</code> if provided.
   <dl>
   <dt><code>^clojure.lang.IPersistentMap options</code></dt>
   <dd>
   See [[random-forest]].<br>
   Overrides <code>:combine</code>, <code>:cost-factory</code>, 
   <code>:feasible</code>, <code>:leaf-learner</code>, and
   <code>:score</code> with the right choices for L2 regression (so the values
   in <code>options</code> will be silently ignored.
   Provides defaults for <code>:mincount</code> and <code>:mtry</code>.
   </dd>
   </dl>"
  
  ^taiga.ensemble.MeanModel [options]
  
  (random-forest (positive-fraction-probability-options options)))
;;------------------------------------------------------------------------------
;; TODO: weighted gini factory
#_(defn minimum-cost-classifier 
    
    ""
    
    ^taiga.ensemble.MinimumCostClassModel [options]
    
    (let [[^double false-negative-cost ^double false-positive-cost] 
          (:misclassification-costs options [0.5 0.5])
          
          false-positive-cost (/ false-positive-cost
                                 (+ false-negative-cost false-positive-cost))
          combiner (fn min-cost-model 
                     ^taiga.ensemble.MinimumCostClassModel [terms]
                     (ensemble/minimum-cost-class-model terms false-positive-cost))]
      (random-forest (assoc (binary-classification-options options)
                            :combine combiner))))
;;------------------------------------------------------------------------------
;; io
;;------------------------------------------------------------------------------
(defn pprint-forest [forest file]
  (with-open [w (z/print-writer file)]
    (binding [*out* w] 
      (pp/pprint (z/clojurize forest)))))
;;------------------------------------------------------------------------------
;; TODO: don't really need these function. Just here to ensure needed readers
;; and classes are loaded.

(defn write-edn 
  
  "Serialize the <code>forest</code> to <code>file</code> in 
   [EDN](https://github.com/edn-format/edn) (clojure) syntax." 
  
  [forest file]
  (z/write-edn forest file))

(defn read-edn
  
  "Read a forest serialized to <code>file</code> in  
   [EDN](https://github.com/edn-format/edn) (clojure) syntax." 
  
  [file]
  (z/read-edn file))
;;------------------------------------------------------------------------------
;; requires cheshire 5.6.x, not in brazil 2016-08-25
#_(defn write-json [forest file]
    (let [pretty (cheshire/create-pretty-printer
                   (assoc cheshire/default-pretty-print-options
                          :indentation 1
                          :indent-arrays? false
                          :indent-objects? false))]
      (with-open [w (z/writer file)] 
        (cheshire/encode-stream forest w {:pretty pretty}))))

(defn write-json 
  "Serialize the <code>forest</code> to <code>file</code> in JSON syntax,
   for the convenience of tree visualization code.
   <br>
   **Use <code>write-edn</code> to serialize trained models so they
   can be de-serialized for later predictions.**
   <br>
   No <code>read-json</code> yet, and may never be." 
  [forest file]
  (with-open [w (z/writer file)] (cheshire/encode-stream forest w)))
;;------------------------------------------------------------------------------