(set! *warn-on-reflection* true)
(set! *unchecked-math* false) ;; warnings in cheshire.generate
(ns ^{:author ["John Alan McDonald"
               "Kristina Lisa Klinkner"
               "wahpenayo at gmail dot com"]
      :date "2018-02-11"
      :doc 
      "A primitive double-valued decision tree leaf." }
    
    taiga.tree.leaf.double
  
  (:require [cheshire.generate]
            [zana.api :as z]
            [taiga.tree.node])
  (:import [taiga.tree.node Node]))
(set! *unchecked-math* :warn-on-boxed)
;;------------------------------------------------------------------------------
;; leaf for classification or regression, not quantile regression
;; TODO: keep count of training records that end up in this leaf.
;; TODO: does this really need to implement Node?

(deftype Leaf [^double score]
  
  Node
  (isLeaf [this] true)
  (trueChild [this] (throw (UnsupportedOperationException.)))
  (falseChild [this] (throw (UnsupportedOperationException.)))
  (child [this predictors datum] (throw (UnsupportedOperationException.)))
  (extract [this predictors] (throw (UnsupportedOperationException.)))
  
  (withChildren [this children] (assert (z/empty? children)) this)
  (isFertile [this] false)
  (getChildren [this] nil)
  
  clojure.lang.IFn$OOD
  (invokePrim ^double [this predictors datum] score)
  
  clojure.lang.IFn
  (invoke [this predictors datum] (.invokePrim this predictors datum))
  
  Object 
  (equals [this that] 
    (and (instance? Leaf that) (== score (.score ^Leaf that)))))
;;------------------------------------------------------------------------------
(defn map->Leaf [m] (Leaf. (:score m)))
(defn map<-Leaf [^Leaf l] {:class :leaf :score (.score l)})
(defmethod z/clojurize Leaf [this] (map<-Leaf this))
(defmethod print-method Leaf [^Leaf this ^java.io.Writer w]
  (if *print-readably*
    (do
      (.write w " #taiga.tree.leaf.double.Leaf{:score ")
      (.write w (Double/toString (.score this)))
      (.write w "} "))
    (.write w (print-str (map<-Leaf this)))))
;;------------------------------------------------------------------------------
;; EDN input (output just works?)
;;------------------------------------------------------------------------------
(z/add-edn-readers! {'taiga.tree.leaf.double.Leaf map->Leaf})
;;------------------------------------------------------------------------------
;; JSON output (input not supported)
;;------------------------------------------------------------------------------
(defn- leaf-encoder [^Leaf l json-generator]
  (cheshire.generate/encode-map (map<-Leaf l) json-generator))
(cheshire.generate/add-encoder Leaf leaf-encoder)
;;------------------------------------------------------------------------------
;; learners
;;------------------------------------------------------------------------------
(defn mean-learner [ground-truth weight]
  (assert (instance? clojure.lang.IFn$OD ground-truth))
  (let [^clojure.lang.IFn$OD calculator 
        (if (and weight (not= z/constantly-1d weight))
          (z/make-calculator 
            z/weighted-mean-accumulator ground-truth weight)
          (z/make-calculator 
            z/mean-accumulator ground-truth))]
    (fn learn-mean-leaf ^taiga.tree.leaf.double.Leaf [data]
      (Leaf. (.invokePrim calculator data)))))
;;------------------------------------------------------------------------------
(defn majority-vote-learner [ground-truth weight]
  (assert (instance? clojure.lang.IFn$OD ground-truth))
  (let [^clojure.lang.IFn$OD calculator 
        (if (and weight (not= z/constantly-1d weight))
          (z/make-calculator 
            z/weighted-majority-vote-accumulator ground-truth weight)
          (z/make-calculator 
            z/majority-vote-accumulator ground-truth))]
    (fn learn-majority-vote-leaf ^taiga.tree.leaf.double.Leaf [data]
      (Leaf. (.invokePrim calculator data)))))
;;------------------------------------------------------------------------------
(defn positive-fraction-learner [ground-truth weight]
  (assert (instance? clojure.lang.IFn$OD ground-truth))
  (let [^clojure.lang.IFn$OD calculator 
        (if (and weight (not= z/constantly-1d weight))
          (z/make-calculator 
            z/weighted-positive-fraction-accumulator ground-truth weight)
          (z/make-calculator 
            z/positive-fraction-accumulator ground-truth))]
    (fn learn-positive-fraction-leaf ^taiga.tree.leaf.double.Leaf [data]
      (Leaf. (.invokePrim calculator data)))))
;;------------------------------------------------------------------------------
