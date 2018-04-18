(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-17"
      :doc "Engel data linear (no intercept) regression models." }
    
    taiga.test.regress.stackloss.linear
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.defs :as defs]
            [taiga.test.regress.stackloss.stackloss :as stackloss]))
;; mvn -Dtest=taiga.test.regress.stackloss.linear clojure:test > stackloss-linear.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
;;----------------------------------------------------------------
(defn- params [model]
  (let [functional (taiga/functional model)
        dual (z/dual functional)
        beta (into [] dual)]
    (println :start beta)
    beta))
;;----------------------------------------------------------------
(defn- coax-q75 [options]
  (println :huber-epsilon (:huber-epsilon options))
  (let [options (assoc options :quantile-p 0.75)
        model (taiga/linear-qr options)
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))]
    (taiga/linear-qr options))) 
;;----------------------------------------------------------------
(defn- coax-l1 [options]
  (println :huber-epsilon (:huber-epsilon options))
  (let [model (taiga/linear-l1  options)
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/linear-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))]
    (taiga/linear-l1  options))) 
;;----------------------------------------------------------------
(defn- test0 [^clojure.lang.IFn fit
              ulps0 ulps1
              true-dual
              true-train-summary]
  (z/seconds 
    nss
    (let [options (assoc (stackloss/options)
                         :minimize? true
                         :max-iterations 10000
                         :initial-bracket-range 1.0e-3
                         :relative-tolerance 1.0e-7
                         :absolute-tolerance 1.0e-7
                         :line-search-relative-tolerance 1.0e-6
                         :line-search-absolute-tolerance 1.0e-6
                         :huber-epsilon 1.0e3
                         :quantile-p 0.5
                         ;;:gradient-check (z/print-writer System/out)
                         :start [0 0 0])
          model (fit options)
          _ (defs/edn-test model (defs/linear-edn-file nss))
          y (:ground-truth (:attributes options))
          xbindings (into (sorted-map)
                          (dissoc (:attributes options) 
                                  :ground-truth :prediction))
          yhat (fn yhat ^double [datum] (model xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          0.75 y yhat (:data options))
          est-dual (z/dual (taiga/functional model))]
      (println "true:\n" true-dual)
      (println "est:\n" est-dual)
      (test/is 
        (z/doubles-approximately== ulps0 true-dual est-dual))
      (test/is 
        (z/maps-approximately== ulps1 true-train-summary train-summary)))))
;;----------------------------------------------------------------
(test/deftest linear-l2-regression 
  (test0 
    taiga/linear-l2-regression
    1.0e1 5.0e1
    (double-array
      [-0.6249932600031913 0.7967652022944222 1.1114224590760979])
    {:rmean -0.1413049503119239
     :rmse 3.762520440804091
     :rmad 3.261681004550787
     :rmqr 3.191028529394825
     :rmrq 4.254704705859766}))
;;----------------------------------------------------------------
(test/deftest linear-l2 
  (test0 
    taiga/linear-l2
    1.0e4 5.0e6
      (double-array
        [-0.6249932600031913 0.7967652022944222 1.1114224590760979])
      {:rmean -0.1413049503119239
       :rmse 3.762520440804091
       :rmad 3.261681004550787
       :rmqr 3.191028529394825
       :rmrq 4.254704705859766}))
;;----------------------------------------------------------------
(test/deftest linear-l1 
  (test0 
    coax-l1
    5.0e12 5.0e12
    (double-array
      [-0.5331620737972911 0.9280709948622143 0.3582438113031284])
    {:rmean -0.1111630079402122
     :rmse 4.073058568858356
     :rmad 3.046262316229623
     :rmqr 2.990680812259517
     :rmrq 3.987574416346023}))
;;----------------------------------------------------------------
(test/deftest linear-q75
  (test0 
    coax-q75
    1.0e14 5.0e13
    (double-array
      [-0.6308261405672007 0.9408138101109738 0.8473489519112208])
    {:rmean -2.771968762844235
     :rmse 4.66272194538081
     :rmad 3.573800716340797
     :rmqr 2.18781633491868
     :rmrq 2.91708844655824}))
;;----------------------------------------------------------------
