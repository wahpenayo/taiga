(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-11-10"
      :doc "Artificial data for random forest unit tests." }
    
    taiga.test.data.record
  
  (:require [clojure.string :as s]
            [clojure.test :as test]
            [clojure.repl :as repl]
            [zana.api :as z]
            [taiga.test.data.kolor :as kolor]
            [taiga.test.data.primate :as primate])
  (:import [taiga.test.java.data Kolor]))
;; mvn -Dtest=taiga.test.data.record clojure:test
;;------------------------------------------------------------------------------
(z/define-datum Record
  [^double x0
   ^double x1
   ^double x2
   ^double x3
   ^double x4
   ^double x5
   ^taiga.test.java.data.Kolor kolor
   ^clojure.lang.Keyword primate
   ^double true-probability
   ^double true-class
   ^double predicted-probability
   ^double predicted-class])
;;------------------------------------------------------------------------------
(def attributes {:x0 x0 :x1 x1 :x2 x2 :x3 x3 :x4 x4 :x5 x5 
                 :kolor kolor :primate primate
                 :ground-truth true-class
                 :prediction predicted-class})
;;------------------------------------------------------------------------------
(defn make-diagonal-step-function [^double bias]
  (assert (<= 0.0 bias 1.0))
  (let [l0 0.0
        w0 1.0
        l1 0.0
        w1 1.0]
    (fn step ^double [^Record datum]
      (let [u0 (/ (- (x0 datum) l0) w0)
            u1 (/ (- (x1 datum) l1) w1)
            diagonal? (or (and (<= u0 0.5) (<= u1 0.5))
                          (and (>= u0 0.5) (>= u1 0.5)))]
        (* bias (if (kolor/primary? (kolor datum))
                  (if diagonal? 1.0 0.0)
                  (if diagonal? 0.0 1.0)))))))
;;------------------------------------------------------------------------------
(defn make-pyramid-function [^double bias]
  (assert (<= 0.0 bias 1.0))
  (let [l0 0.0
        w0 1.0
        l1 0.0
        w1 1.0]
    (fn pyramid ^double [^Record datum]
      (let [u0 (/ (- (x0 datum) l0) w0)
            _(assert (<= 0.0 u0 1.0))
            u1 (/ (- (x1 datum) l1) w1)
            _(assert (<= 0.0 u1 1.0))
            u (* 0.5 (+ (Math/min u0 (- 1.0 u0))
                        (Math/min u1 (- 1.0 u1))))
            _(assert (<= 0.0 u 1.0))
            p (* bias (if (kolor/primary? (kolor datum)) u (- 1.0 u)))]
        (assert (<= 0.0 p 1.0))
        p))))
;;------------------------------------------------------------------------------
(defn make-spikes-function [^double bias]
  (assert (<= 0.0 bias 1.0))
  (fn spikes ^double [^Record datum]
    (let [u0 (x0 datum) _ (assert (<= 0.0 u0 1.0))
          u1 (x1 datum) _ (assert (<= 0.0 u1 1.0))
          k (kolor datum)
          [^double mu0 ^double mu1] (cond (= k Kolor/RED)     [0.25 0.26]
                                          (= k Kolor/GREEN)   [0.51 0.27]
                                          (= k Kolor/BLUE)    [0.78 0.28]
                                          (= k Kolor/CYAN)    [0.29 0.79]
                                          (= k Kolor/MAGENTA) [0.52 0.74]
                                          (= k Kolor/YELLOW)  [0.68 0.73])
          v0 (- mu0 u0)
          v1 (- mu1 u1)
          r (Math/sqrt (+ (* v0 v0) (* v1 v1)))
          a 0.3
          a-r (Math/max 0.0 (- a r))
          p (* bias (/ (* a-r a-r) (* a a)))] 
      (assert (<= 0.0 p 1.0))
      p)))
;;------------------------------------------------------------------------------
(defn make-cone-function [^double bias]
  (assert (<= 0.0 bias 1.0))
  (let [l0 0.0
        w0 1.0
        l1 0.0
        w1 1.0]
    (fn cone ^double [^Record datum]
      (let [u0 (/ (- (x0 datum) l0) w0)
            u0 (- (* 2.0 u0) 1.0)
            u1 (/ (- (x1 datum) l1) w1)
            u1 (- (* 2.0 u1) 1.0)
            u (- 1.0 (* 0.5 (+ (* u0 u0) (* u1 u1))))]
        (if (kolor/primary? (kolor datum)) u  (- 1.0 u))))))
;;------------------------------------------------------------------------------
;; TODO: queue?
(def ^:private seed0 "7CFA49EF4DF2F09D5F58B2F9198D4211")
(def ^:private seed1 "38268047490594E3278EEA0E5182D7A2")
(def ^:private seed2 "7DFBA14AB78F049E5DAA711428C8CBEF")
(def ^:private seed3 "0B4773B2C811374096880EA2B045E6EF")
(def ^:private seed4 "087E31A05DDF780B29574B4E83D92615")
(def ^:private seed5 "0751BC45236182BDE2C6E92A513CF383")
(def ^:private seed6 "28EE35787E1481D72D901EF53516B3A6")
(def ^:private seed7 "2D3DF75EC1459A4B26C0B29449D959CC")
(def ^:private seed8 "BD504A2D507F96F63889FFA3A1EBBAC6")
;;------------------------------------------------------------------------------
(defn generator [^clojure.lang.IFn$OD prob]
  
  (let [^clojure.lang.IFn$D generate-x0 (z/continuous-uniform-generator seed0)
        ^clojure.lang.IFn$D generate-x1 (z/continuous-uniform-generator seed1)
        ;;^clojure.lang.IFn$D generate-x2 (z/continuous-uniform-generator seed2)
        ;;^clojure.lang.IFn$D generate-x3 (z/continuous-uniform-generator seed3)
        ;;^clojure.lang.IFn$D generate-x4 (z/continuous-uniform-generator seed4)
        ;;^clojure.lang.IFn$D generate-x5 (z/continuous-uniform-generator seed5)
        generate-kolor (kolor/generator seed6)
        generate-primate (primate/generator seed7)
        generate-01 (z/bernoulli-generator seed8)]
    
    (fn random-record ^Record [_]
      (let [x0 (.invokePrim generate-x0)
            x1 (.invokePrim generate-x1)
            x2 (Math/min x0 x1) ;;(.invokePrim generate-x2)
            x3 (Math/max x0 x1) ;;(.invokePrim generate-x4)
            x4 (+ x0 x1) ;;(.invokePrim generate-x4)
            x5 (- x0 x1) ;;(.invokePrim generate-x5)
            kolor (generate-kolor)
            primate (generate-primate)
            datum (Record. x0 x1 x2 x3 x4 x5 kolor primate 
                           Double/NaN Double/NaN Double/NaN Double/NaN)
            p (prob datum)
            _(assert (<= 0.0 p 1.0))
            c (generate-01 p)]
        (assoc datum :true-class c :true-probability p)))))
;;------------------------------------------------------------------------------
(defn enum-valued? [f]
  (println)
  (println f)
  (let [[ns n] (s/split (pr-str (class f)) #"\$")
        s (symbol ns n)
        r (resolve s)
        m (meta r)
        a (first (:arglists m))
        tag (:tag (meta a))
        codomain (resolve tag)
        tf (and (instance? Class codomain) (.isEnum ^Class codomain))
        ]
  (prn ns n)
  (prn (class s) s)
  (prn (class r) r)
  (prn a)
  (prn (class codomain) codomain)
  (prn tf)
  tf))  
;;------------------------------------------------------------------------------
(test/deftest enum-codomain
 (enum-valued? x0) 
 (enum-valued? kolor) 
 (enum-valued? primate)
 )