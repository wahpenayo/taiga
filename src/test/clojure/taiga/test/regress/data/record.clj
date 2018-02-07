(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-02-06"
      :doc "Artificial data for regression unit tests." }
    
    taiga.test.regress.data.record
  
  (:require [clojure.string :as s]
            [clojure.test :as test]
            [clojure.repl :as repl]
            [zana.api :as z]
            [taiga.test.regress.data.kolor :as kolor]
            [taiga.test.regress.data.primate :as primate])
  (:import [clojure.lang IFn$D IFn$OD Keyword]
           [taiga.test.java.data Kolor]))
;;----------------------------------------------------------------
(z/define-datum Record
  [^double x0
   ^double x1
   ^double x2
   ^double x3
   ^double x4
   ^double x5
   ^Kolor kolor
   ^Keyword primate
   ^double mean
   ^double y
   ^double predicted-mean
   ^double predicted-y])
;;----------------------------------------------------------------
(def attributes {:x0 x0 :x1 x1 :x2 x2 :x3 x3 :x4 x4 :x5 x5 
                 :kolor kolor :primate primate
                 :ground-truth y
                 :prediction predicted-y})
(def x-attributes 
  (vals (dissoc attributes :ground-truth :prediction)))
;;----------------------------------------------------------------
(def ^:private seed0 "C36A87179446D2CB0CD70272CFD1E1CA")
(def ^:private seed1 "D56C1509ED79A73017F415BB78103B36")
(def ^:private seed2 "AEC68E8582EFF7A9932DAAB33DC4559A")
(def ^:private seed3 "27035B01F9BAC199CEE88C6AE5E45928")
(def ^:private seed4 "C6B3763CEF7C517A3CD3BEFDA7C17A2C")
(def ^:private seed5 "064D88D1CDD6F45D64BE846E22141C49")
(def ^:private seed6 "1A0792C6EAFE479722508D723D1550A0")
(def ^:private seed7 "E71CC2795D5DB03C94CEB878AC0C886C")
(def ^:private seed8 "5C53D355CED2EA5D3888CBAD069C9D89")
(def ^:private seed9 "888CD02BF903BB078E640090A0F23FF8")
;;----------------------------------------------------------------
(defn generator [^IFn$OD mean]
  (let [^IFn$D generate-x0 (z/continuous-uniform-generator -1.0 1.0 seed0)
        ^IFn$D generate-x1 (z/continuous-uniform-generator -1.0 1.0 seed1)
        ^IFn$D generate-x2 (z/continuous-uniform-generator -1.0 1.0 seed2)
        ^IFn$D generate-x3 (z/continuous-uniform-generator -1.0 1.0 seed3)
        ^IFn$D generate-x4 (z/continuous-uniform-generator -1.0 1.0 seed4)
        ^IFn$D generate-x5 (z/continuous-uniform-generator -1.0 1.0 seed5)
        generate-kolor (kolor/generator seed6)
        generate-primate (primate/generator seed7)
        ^IFn$D generate-dy (z/continuous-uniform-generator -1.0 1.0 seed8)]
    (fn random-record ^Record [_]
      (let [x0 (.invokePrim generate-x0)
            x1 (.invokePrim generate-x1)
            x2 #_(Math/min x0 x1) (.invokePrim generate-x2)
            x3 #_(Math/max x0 x1) (.invokePrim generate-x4)
            x4 #_(+ x0 x1) (.invokePrim generate-x4)
            x5 #_(- x0 x1) (.invokePrim generate-x5)
            kolor (generate-kolor)
            primate (generate-primate)
            datum (Record. x0 x1 x2 x3 x4 x5 kolor primate 
                           Double/NaN Double/NaN 
                           Double/NaN Double/NaN)
            mu (.invokePrim mean datum)
            y  (+ mu (.invokePrim generate-dy))]
        (assoc datum :mean mu :y y)))))
;;----------------------------------------------------------------
(defn make-pyramid-function [^double scale]
  (fn p ^double [^Record datum]
    (let [mu (* scale 
                (+ (Math/abs (x0 datum)) (Math/abs (x1 datum))))]
      (if (kolor/primary? (kolor datum)) 
        mu
        (- mu)))))
;;----------------------------------------------------------------
(defn make-affine-function [^double scale]
  (let [^IFn$D generate-dy (z/continuous-uniform-generator -100.0 100.0 seed8)]
    (fn p ^double [^Record datum]
      (let [mu (* scale 
                  (+ (Math/abs (x0 datum)) (Math/abs (x1 datum))))]
        (if (kolor/primary? (kolor datum)) 
          mu
          (- mu))))))
;;----------------------------------------------------------------
