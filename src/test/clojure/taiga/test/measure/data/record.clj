(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :since "2017-10-27"
      :date "2017-11-05"
      :doc "Artificial data for random forest unit tests.
            around simple regression function." }
    
    taiga.test.measure.data.record
  
  (:require [clojure.string :as s]
            [clojure.test :as test]
            [clojure.repl :as repl]
            [zana.api :as z]
            [taiga.test.measure.data.kolor :as kolor]
            [taiga.test.measure.data.primate :as primate])
  (:import [org.apache.commons.math3.random 
            RandomGenerator Well44497b]
           [org.apache.commons.math3.distribution 
            NormalDistribution RealDistribution 
            UniformRealDistribution]
           [taiga.test.java.data Kolor]))
;; mvn -Dtest=taiga.test.measure.data.record clojure:test
;;----------------------------------------------------------------
(z/define-datum Record
  [^double x0
   ^double x1
   ^double x2
   ^double x3
   ^double x4
   ^double x5
   ^taiga.test.java.data.Kolor kolor
   ^clojure.lang.Keyword primate
   ^double y
   ^double q25
   ^double q50
   ^double q75
   ^double yhat
   ^double q25hat
   ^double q50hat
   ^double q75hat])
;;----------------------------------------------------------------
(def attributes {:x0 x0 :x1 x1 :x2 x2 :x3 x3 :x4 x4 :x5 x5 
                 :kolor kolor :primate primate
                 :ground-truth y :prediction yhat})
;;----------------------------------------------------------------
(defn make-pyramid-function [^double scale]
  (fn median ^double [^Record datum]
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
(defn generator [^clojure.lang.IFn$OD median]
  (let [^clojure.lang.IFn$D generate-x0 (z/continuous-uniform-generator -1.0 1.0 seed0)
        ^clojure.lang.IFn$D generate-x1 (z/continuous-uniform-generator -1.0 1.0 seed1)
        ^clojure.lang.IFn$D generate-x2 (z/continuous-uniform-generator -1.0 1.0 seed2)
        ^clojure.lang.IFn$D generate-x3 (z/continuous-uniform-generator -1.0 1.0 seed3)
        ^clojure.lang.IFn$D generate-x4 (z/continuous-uniform-generator -1.0 1.0 seed4)
        ^clojure.lang.IFn$D generate-x5 (z/continuous-uniform-generator -1.0 1.0 seed5)
        generate-kolor (kolor/generator seed6)
        generate-primate (primate/generator seed7)
        ^ints well44497b-seed (z/seed "seeds/Well44497b-2017-11-05-00.edn")
        ^RandomGenerator prng (Well44497b. well44497b-seed)
        ;;^RealDistribution dymu (NormalDistribution. prng 0.0 1.0)
        ^RealDistribution dymu (UniformRealDistribution. prng -1.0 1.0)
        ^clojure.lang.IFn$D dy (fn dy ^double [] (.sample dymu))]
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
                    Double/NaN Double/NaN Double/NaN Double/NaN
                    Double/NaN Double/NaN Double/NaN Double/NaN)
            m (.invokePrim median datum)
            y  (+ m (.invokePrim dy))
            q25 (+ m (z/quantile dymu 0.25))
            q50 (+ m (z/quantile dymu 0.50))
            q75 (+ m (z/quantile dymu 0.75))]
        (assoc datum :y y :q25 q25 :q50 q50 :q75 q75)))))
;;----------------------------------------------------------------
