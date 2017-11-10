(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :since "2017-11-09"
      :date "2017-11-09"
      :doc "Quantile record classes, to hold predicted values 
            for testing." }
    
    taiga.test.measure.data.quartiles
  
  (:require [zana.api :as z])
  (:import [clojure.lang IFn$ODD]
           [org.apache.commons.math3.distribution 
            RealDistribution]))
;;----------------------------------------------------------------
;; TODO: implement RealDistribution?
(z/define-datum Quartiles
  [^double q25
   ^double q50
   ^double q75])
;;----------------------------------------------------------------
(defn quartiles ^Quartiles [^IFn$ODD quantile datum]
  (Quartiles.
    (quantile datum 0.25)
    (quantile datum 0.50)
    (quantile datum 0.75)))
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
    (instance? Quartiles d)
    (let [^Quartiles d d]
      (+ (qcost y (q25 d) 0.25)
         (qcost y (q50 d) 0.50)
         (qcost y (q75 d) 0.75)))
    
    (instance? RealDistribution d)
    (let [^RealDistribution d d]
      (+ (qcost y (z/quantile d 0.25) 0.25)
         (qcost y (z/quantile d 0.50) 0.50)
         (qcost y (z/quantile d 0.75) 0.75)))
    
    :else
    (throw (IllegalArgumentException.
             (print-str
               "can't compute decile cost for " (class d))))))
;;----------------------------------------------------------------
