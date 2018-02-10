(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-02-08"
      :doc "Artificial data for regression unit tests." }
    
    taiga.test.regress.data.xy
  
  (:require [clojure.string :as s]
            [clojure.test :as test]
            [clojure.repl :as repl]
            [zana.api :as z]
            [taiga.test.regress.data.kolor :as kolor]
            [taiga.test.regress.data.primate :as primate])
  (:import [clojure.lang IFn$D IFn$OD Keyword]
           [taiga.test.java.data Kolor]))
;;----------------------------------------------------------------
(z/define-datum XY
  [^double x
   ^double mean
   ^double y
   ^double predicted-mean
   ^double predicted-y])
;;----------------------------------------------------------------
(def ^:private seed00 "C36A87179446D2CB0CD70272CFD1E1CA")
(def ^:private seed01 "D56C1509ED79A73017F415BB78103B36")
;;----------------------------------------------------------------
(defn generator [^IFn$OD mean ^double sigma]
  (let [^IFn$D gx (z/continuous-uniform-generator 
                    -100.0 100.0 seed00)
        ^IFn$D gdy (if (< 0.0 sigma)
                     (z/gaussian-generator 0.0 sigma seed01)
                     ;; else noiseless constant 0.0 error
                     z/constantly-0d)]
    (fn random-record ^XY [_]
      (let [x (.invokePrim gx)
            datum (XY. x 
                       Double/NaN Double/NaN 
                       Double/NaN Double/NaN)
            mu (.invokePrim mean datum)
            y  (+ mu (.invokePrim gdy))]
        (assoc datum :mean mu :y y)))))
;;----------------------------------------------------------------
(def attributes {:x x 
                 :ground-truth y
                 :prediction predicted-y})
(def xbindings 
  (into (sorted-map)
        (dissoc attributes :ground-truth :prediction)))
;;----------------------------------------------------------------
(defn make-xy-function [^double slope ^double intercept]
    (fn y-mean ^double [^XY datum]
      (+ intercept (* slope (.x datum)))))
;;----------------------------------------------------------------
