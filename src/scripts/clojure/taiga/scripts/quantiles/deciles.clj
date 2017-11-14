(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :since "2017-11-10"
      :date "2017-11-10"
      :doc "Quantile record class, to hold predicted values 
            for benchmarking." }
    
    taiga.scripts.quantiles.deciles
  
  (:require [zana.api :as z])
  (:import [clojure.lang IFn$ODD]
           [org.apache.commons.math3.distribution 
            RealDistribution]))
;;----------------------------------------------------------------
;; TODO: implement RealDistribution?
(z/define-datum Deciles
  [^double q10
   ^double q20
   ^double q30
   ^double q40
   ^double q50
   ^double q60
   ^double q70
   ^double q80
   ^double q90])
;;----------------------------------------------------------------
(defn make ^Deciles [^IFn$ODD quantile d]
  (Deciles.
    (quantile d 0.10)
    (quantile d 0.20)
    (quantile d 0.30)
    (quantile d 0.40)
    (quantile d 0.50)
    (quantile d 0.60)
    (quantile d 0.70)
    (quantile d 0.80)
    (quantile d 0.90)))
;;----------------------------------------------------------------
(defn- qcost ^double [^double y ^double q ^double p]
  (let [y-q (- y q)]
    (if (<= 0.0 y-q)
      (* 0.5 (/ y-q (- 1 p)))
      (* 0.5 (/ y-q (- p))))))
;;----------------------------------------------------------------
;; TODO: defmulti?
(defn cost ^double [^double y d]
  (cond 
    (instance? Deciles d)
    (+ (qcost y (q10 d) 0.10)
       (qcost y (q20 d) 0.20)
       (qcost y (q30 d) 0.30)
       (qcost y (q40 d) 0.40)
       (qcost y (q50 d) 0.50)
       (qcost y (q60 d) 0.60)
       (qcost y (q70 d) 0.70)
       (qcost y (q80 d) 0.80)
       (qcost y (q90 d) 0.90))
    
    (instance? RealDistribution d)
    (+ (qcost y (z/quantile d 0.10) 0.10)
       (qcost y (z/quantile d 0.20) 0.20)
       (qcost y (z/quantile d 0.30) 0.30)
       (qcost y (z/quantile d 0.40) 0.40)
       (qcost y (z/quantile d 0.50) 0.50)
       (qcost y (z/quantile d 0.60) 0.60)
       (qcost y (z/quantile d 0.70) 0.70)
       (qcost y (z/quantile d 0.80) 0.80)
       (qcost y (z/quantile d 0.90) 0.90))
    
    :else
    (throw (IllegalArgumentException.
             (print-str
               "can't compute decile cost for " (class d))))))
;;----------------------------------------------------------------
