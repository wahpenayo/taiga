(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" 
      :since "2017-01-13"
      :date "2017-11-08"
      :doc "Ensemble (Reducer) model classes." }
    
    taiga.ensemble
  
  (:require [zana.api :as z])
  (:import [java.util IdentityHashMap List Map]
           [clojure.lang IFn IFn$OOD]
           [zana.java.prob WEPDF]))
;;----------------------------------------------------------------
;; TODO: move to Java so we can inherit IFn$OOD
;; TODO: have a single EnsembleModel class, parameterized by an
;; Accumulator factory. Issue: serialization of factory function 
;; Accumulator instances to edn.
;;----------------------------------------------------------------
(definterface EnsembleModel
  (^java.util.Map parameters [])
  (^java.util.List terms [])
  (^long nterms []))

(defn parameters 
  "Return a  map of the parameters input used in training."
  ^Map [^EnsembleModel model] (.parameters model))

(defn terms 
  "Return an <code>Iterable</code> over the terms of the ensemble 
   model, each a model function itself."
  ^List [^EnsembleModel model] (.terms model))
(defn nterms 
  "How many terms in the ensemble model?"
  ^long [^EnsembleModel model] (.nterms model))
;;----------------------------------------------------------------
(defrecord MeanModel [^Map params
                      ^List terms]
  EnsembleModel
  (parameters [this] params)
  (terms [this] terms)
  (nterms [this] (z/count terms))
  IFn$OOD
  (invokePrim ^double [this predictors datum]
    ;; not strictly right -- nil might be a valid input
    (assert (not (nil? datum)))
    (assert (instance? Map predictors))
    (let [m (z/mean-accumulator)]
      (z/mapc 
        (fn [^IFn$OOD term] 
          (.add m (.invokePrim term predictors datum)))
        terms)
      (.doubleValue m)))
  IFn
  (invoke [this predictors datum] (.invokePrim this predictors datum)))
;;----------------------------------------------------------------
;; TODO: copy terms for safety?
(defn mean-model
  (^taiga.ensemble.MeanModel [^Map params ^List terms]
    (MeanModel. params terms))
  (^taiga.ensemble.MeanModel [^List terms]
    (mean-model {} terms)))
;;----------------------------------------------------------------
(defrecord MeanVectorModel [^Map params
                            ^long codimension
                            ^List terms]
  EnsembleModel
  (parameters [this] params)
  (terms [this] terms)
  (nterms [this] (z/count terms))
  IFn
  (invoke [this predictors datum]
    ;; not strictly right -- nil might be a valid input
    (assert (not (nil? datum)))
    (assert (instance? Map predictors))
    (let [^zana.java.accumulator.Accumulator m (z/vector-mean-accumulator codimension)]
      (z/mapc (fn [^IFn term] 
                (let [yhat (term predictors datum)
                      aclass (class (double-array 0))]
                  (assert (instance? aclass yhat) 
                          (print-str (class term) " -> " (class yhat)))
                  (.add m ^Object yhat)))
              terms)
      (.value m))))
;;----------------------------------------------------------------
;; TODO: copy terms for safety?
(defn mean-vector-model
  (^taiga.ensemble.MeanModel [^Map params 
                              codimension 
                              ^List terms]
    (MeanVectorModel. params codimension terms))
  (^taiga.ensemble.MeanModel [codimension 
                              ^List terms]
    (mean-vector-model {} codimension terms)))
;;----------------------------------------------------------------
(defrecord MinimumCostClassModel [^Map params
                                  ^List terms
                                  ^double false-positive-cost]
  EnsembleModel
  (parameters [this] params)
  (terms [this] terms)
  (nterms [this] (z/count terms))
  IFn$OOD
  (invokePrim ^double [this predictors datum]
    (assert (instance? Map predictors))
    ;; not strictly right -- nil might be a valid input
    (assert (not (nil? datum)))
    (let [^zana.java.accumulator.Accumulator m 
          (z/minimum-expected-cost-class-accumulator false-positive-cost)
          ;; for speed?
          p (IdentityHashMap. ^Map predictors)]
      (z/mapc (fn [^IFn$OOD term]
                (let [yhati (.invokePrim term p datum)]
                  (.add m yhati)))
              terms)
      (.doubleValue m)))
  IFn
  (invoke [this predictors datum] (.invokePrim this predictors datum)))
;;----------------------------------------------------------------
;; TODO: copy terms for safety?
(defn minimum-cost-class-model
  (^taiga.ensemble.MinimumCostClassModel [^Map params
                                          ^List terms
                                          ^double false-positive-cost]
    (assert (< 0.0 false-positive-cost 1.0))
    (MinimumCostClassModel. params terms false-positive-cost))
  (^taiga.ensemble.MinimumCostClassModel [^List terms
                                          ^double false-positive-cost]
    (minimum-cost-class-model {} terms false-positive-cost)))
;;----------------------------------------------------------------
;; TODO: copy terms for safety?
(defn majority-model
  (^taiga.ensemble.MinimumCostClassModel [^Map params
                                          ^List terms]
    (minimum-cost-class-model params terms 0.5))
  (^taiga.ensemble.MinimumCostClassModel [^List terms]
    (majority-model {} terms)))
;;----------------------------------------------------------------
(defrecord PositiveFractionModel [^Map params
                                  ^List terms]
  EnsembleModel
  (parameters [this] params)
  (terms [this] terms)
  (nterms [this] (z/count terms))
  IFn$OOD
  (invokePrim ^double [this predictors datum]
    (assert (instance? Map predictors))
    ;; not strictly right -- nil might be a valid input
    (assert (not (nil? datum)))
    (let [m (z/positive-fraction-accumulator)
          ;; for speed?
          p (IdentityHashMap. ^Map predictors)]
      (z/mapc
        (fn [^IFn$OOD term]
          (let [yhati (.invokePrim term p datum)]
            (.add m yhati)))
        terms)
      (.doubleValue m)))
  IFn
  (invoke [this predictors datum] (.invokePrim this predictors datum)))
;;----------------------------------------------------------------
;; TODO: copy terms for safety?
(defn positive-fraction-model
  (^taiga.ensemble.PositiveFractionModel [^Map params
                                          ^List terms]
    (PositiveFractionModel. params terms))
  (^taiga.ensemble.PositiveFractionModel [^List terms]
    (positive-fraction-model {} terms)))
;;----------------------------------------------------------------
;; TODO: add as method to interface or as generic function
(defn take-terms 
  "Return a model using just the first <code>n</code> from this ensemble model.
   <br>
   If the model has <code>n</code> terms or fewer, the model itself is returned."
  [^long n ^EnsembleModel model]
  (assert (>= (nterms model) n 0))
  (if (== n (nterms model))
    model
    (let [terms (z/take n (terms model))]
      (cond (instance? PositiveFractionModel model) 
            (positive-fraction-model (parameters model) terms)
            (instance? MinimumCostClassModel model) 
            (minimum-cost-class-model (parameters model) terms)
            (instance? MeanModel model)
            (mean-model (parameters model) terms)))))
;;----------------------------------------------------------------
;; Predicted value is a RealDistribution
(defrecord RealDistributionModel [^Map params
                                  ^List terms]
  EnsembleModel
  (parameters [this] params)
  (terms [this] terms)
  (nterms [this] (z/count terms))
  IFn
  (invoke [this predictors datum]
    ;; not strictly right -- nil might be a valid input
    (assert (not (nil? datum)))
    (assert (instance? Map predictors))
    ;; NOTE: assuming rpms are all WEPDF
    ;; NOTE: handle the case of no training data ending up
    ;; in a leaf in a forest model by returning nil from that
    ;; ensemble term (a tree) and ignoring nils when averaging the
    ;; rpms.
    (let [rpms (z/keep-map ;; drops nils
                           (fn [^IFn term] (term predictors datum))
                           terms)]
      (WEPDF/average rpms))))
;;----------------------------------------------------------------
;; TODO: copy terms for safety?
;; TODO: add :arglists to metadata
(defn probability-measure-model
  (^taiga.ensemble.RealDistributionModel 
   [^Map params ^List terms]
    (RealDistributionModel. params terms))
  (^taiga.ensemble.RealDistributionModel 
   [^List terms]
    (probability-measure-model {} terms)))
;;----------------------------------------------------------------
(z/add-edn-readers!
  {'taiga.ensemble.PositiveFractionModel 
   map->PositiveFractionModel
   
   'taiga.ensemble.MinimumCostClassModel
   map->MinimumCostClassModel
   
   'taiga.ensemble.MeanVectorModel
   map->MeanVectorModel
   
   'taiga.ensemble.MeanModel 
   map->MeanModel
   
   'taiga.ensemble.RealDistributionModel 
   map->RealDistributionModel})
;;----------------------------------------------------------------