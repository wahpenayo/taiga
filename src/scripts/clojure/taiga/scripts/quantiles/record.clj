(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :since "2017-11-10"
      :date "2017-11-10"
      :doc "Artificial data for random forest unit tests.
            around simple regression function." }
    
    taiga.scripts.quantiles.record
  
  (:require [clojure.string :as s]
            [clojure.test :as test]
            #_[clojure.repl :as repl]
            [zana.api :as z]
            [taiga.scripts.quantiles.kolor :as kolor]
            [taiga.scripts.quantiles.primate :as primate]
            [taiga.scripts.quantiles.deciles :as deciles])
  (:import [clojure.lang IFn$ODD]
           [org.apache.commons.math3.random 
            RandomGenerator Well44497b]
           [org.apache.commons.math3.distribution 
            NormalDistribution RealDistribution 
            UniformRealDistribution]
           [zana.java.prob TranslatedRealDistribution]
           [taiga.test.java.data Kolor]
           [taiga.scripts.quantiles.deciles Deciles]))
;; mvn -Dtest=taiga.scripts.quantiles.record clojure:test
;;----------------------------------------------------------------
(z/define-datum Record
  [;; predictors
   ^double x0
   ^double x1
   ^double x2
   ^double x3
   ^double x4
   ^double x5
   ^taiga.test.java.data.Kolor kolor
   ^clojure.lang.Keyword primate
   ;; ground truth
   [^RealDistribution ymu 
   ^double y ;; a sample from ymu
   ;; predictions 
   ;; predicted empirical distributions too large to keep
   ;; could change with a more compact approximation
   ^Deciles qhat ;; measure regression prediction
   ])
;;----------------------------------------------------------------
;(defn mean ^double [^Record datum]
;  (.getNumericalMean (ymu datum)))
;(defn variance ^double [^Record datum]
;  (.getNumericalVariance (ymu datum)))
;(defn std ^double [^Record datum]
;  (Math/sqrt (variance datum)))
;(defn quantile ^double [^Record datum ^double p]
;  (.inverseCumulativeProbability (ymu datum) p))
(defn quantile ^double [^Record datum ^double p]
  (z/quantile (ymu datum) p))
(defn q10 ^double [^Record datum] (quantile datum 0.10))
(defn q20 ^double [^Record datum] (quantile datum 0.20))
(defn q30 ^double [^Record datum] (quantile datum 0.30))
(defn q40 ^double [^Record datum] (quantile datum 0.40))
(defn q50 ^double [^Record datum] (quantile datum 0.50))
(defn q60 ^double [^Record datum] (quantile datum 0.60))
(defn q70 ^double [^Record datum] (quantile datum 0.70))
(defn q80 ^double [^Record datum] (quantile datum 0.80))
(defn q90 ^double [^Record datum] (quantile datum 0.90))
(defn q10hat ^double [^Record datum] (deciles/q10 (qhat datum)))
(defn q20hat ^double [^Record datum] (deciles/q20 (qhat datum)))
(defn q30hat ^double [^Record datum] (deciles/q30 (qhat datum)))
(defn q40hat ^double [^Record datum] (deciles/q40 (qhat datum)))
(defn q50hat ^double [^Record datum] (deciles/q50 (qhat datum)))
(defn q60hat ^double [^Record datum] (deciles/q60 (qhat datum)))
(defn q70hat ^double [^Record datum] (deciles/q70 (qhat datum)))
(defn q80hat ^double [^Record datum] (deciles/q80 (qhat datum)))
(defn q90hat ^double [^Record datum] (deciles/q90 (qhat datum)))
;;----------------------------------------------------------------
(def attributes {:x0 x0 :x1 x1 :x2 x2 :x3 x3 :x4 x4 :x5 x5 
                 :kolor kolor :primate primate
                 :ground-truth y :prediction q50hat})
(def tsv-attributes 
  (assoc 
    attributes
    ;; :mean mean :std std
    ;; :meanhat meanhat :stdhat stdhat
    :q10 q10 :q20 q20 :q30 q30 
    :q40 q40 :q50 q50 :q60 q60
    :q70 q70 :q80 q80 :q90 q90
    :q10hat q10hat :q20hat q20hat :q30hat q30hat 
    :q40hat q40hat :q50hat q50hat :q60hat q60hat
    :q70hat q70hat :q80hat q80hat :q90hat q90hat))
;;----------------------------------------------------------------
(defn make-pyramid-function [^double scale]
  (fn dz ^double [^Record datum]
    (let [m (* scale 
               (+ (Math/abs (x0 datum)) (Math/abs (x1 datum))))]
      (if (kolor/primary? (kolor datum)) m (- m)))))
;;----------------------------------------------------------------
(def ^:private seed0 "D716723F3CC3D31912C13D5B2D0ABA43")
(def ^:private seed1 "CFCC37706F1F0CDF1706E8F29A18AF4D")
(def ^:private seed2 "41FDFB437B7700C65825FC71D973A30E")
(def ^:private seed3 "84937A916065659FD9E93C035D677FB3")
(def ^:private seed4 "6599D5221492A6CBAD19BBB07007CA4E")
(def ^:private seed5 "3B46192B2D955A9D1D0E7E85542EE6E4")
(def ^:private seed6 "B9BEEEEF9B5A8098279CEBAA8512B5B2")
(def ^:private seed7 "607920A177E59A652A95AB868A4EE77B")
(def ^:private seed8 "97C4E9B1671CFEC7FCC9EADA50A1E5A8")
;;----------------------------------------------------------------
(defn generator [^clojure.lang.IFn$OD center]
  (let [^clojure.lang.IFn$D generate-x0 (z/continuous-uniform-generator -1.0 1.0 seed0)
        ^clojure.lang.IFn$D generate-x1 (z/continuous-uniform-generator -1.0 1.0 seed1)
        ^clojure.lang.IFn$D generate-x2 (z/continuous-uniform-generator -1.0 1.0 seed2)
        ^clojure.lang.IFn$D generate-x3 (z/continuous-uniform-generator -1.0 1.0 seed3)
        ^clojure.lang.IFn$D generate-x4 (z/continuous-uniform-generator -1.0 1.0 seed4)
        ^clojure.lang.IFn$D generate-x5 (z/continuous-uniform-generator -1.0 1.0 seed5)
        generate-kolor (kolor/generator seed6)
        generate-primate (primate/generator seed7)
        ^RandomGenerator prng (z/well44497b "seeds/Well44497b-2017-11-05-00.edn")
        ;;^RealDistribution mu (NormalDistribution. prng 0.0 1.0)
        ^RealDistribution mu (UniformRealDistribution. prng -1.0 1.0)]
    
    ;; TODO: create a generating y distribution at every sample.
    ;; and serialize that, rather than quantiles.
    ;; Would want to have an interface for transforming distributions...
    (fn random-record ^Record [_]
      (let [x0 (.invokePrim generate-x0)
            x1 (.invokePrim generate-x1)
            x2 #_(Math/min x0 x1) (.invokePrim generate-x2)
            x3 #_(Math/max x0 x1) (.invokePrim generate-x4)
            x4 #_(+ x0 x1) (.invokePrim generate-x4)
            x5 #_(- x0 x1) (.invokePrim generate-x5)
            kolor (generate-kolor)
            primate (generate-primate)
            datum (Record. 
                    x0 x1 x2 x3 x4 x5 kolor primate
                    nil Double/NaN nil)
            dy (.invokePrim center datum)
            ^RealDistribution ymu (TranslatedRealDistribution/shift
                                    mu dy)
            y  (.sample ymu)]
        (assoc datum :ymu ymu :y y)))))
;;----------------------------------------------------------------
