(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-17"
      :doc "Engel data affine regression models." }
    
    taiga.test.regress.engel.affine
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.defs :as defs]
            [taiga.test.regress.engel.engel :as engel]))
;; mvn -Dtest=taiga.test.regress.engel.affine clojure:test > engel-affine.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
;;----------------------------------------------------------------
(defn- params [model]
  (let [functional (taiga/functional model)
        dual (z/dual (z/linear-part functional))
        translation (z/translation functional)
        beta (conj (into [] dual) translation)]
    (println :start beta)
    beta))
;;----------------------------------------------------------------
(defn- coax-q75 [options]
  (println :huber-epsilon (:huber-epsilon options))
  (let [options (assoc options :quantile-p 0.75)
        model (taiga/affine-qr options)
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-qr options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))]
    (taiga/affine-qr options))) 
;;----------------------------------------------------------------
(defn- coax-l1 [options]
  (println :huber-epsilon (:huber-epsilon options))
  (let [model (taiga/affine-l1  options)
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))
        model (taiga/affine-l1  options) 
        options (assoc options
                       :huber-epsilon (* 0.1 (double (:huber-epsilon options)))
                       :start (params model))
        #_(println :huber-epsilon (:huber-epsilon options))]
    (taiga/affine-l1  options))) 
;;----------------------------------------------------------------
(defn- test0 [^clojure.lang.IFn fit
              ulps0 ulps1 ulps2 
              true-translation
              true-dual
              true-train-summary]
  (z/seconds 
    nss
    (let [options (assoc (engel/options)
                         :minimize? true
                         :max-iterations 10000
                         :initial-bracket-range 1.0e-3
                         :relative-tolerance 1.0e-7
                         :absolute-tolerance 1.0e-7
                         :line-search-relative-tolerance 1.0e-7
                         :line-search-absolute-tolerance 1.0e-7
                         :huber-epsilon 1.0e3
                         :quantile-p 0.5
                         ;;:gradient-check (z/print-writer System/out)
                         :start [0 0])
          model (fit options)
          _ (defs/edn-test model (defs/affine-edn-file nss))
          y (:ground-truth (:attributes options))
          xbindings (into (sorted-map)
                          (dissoc (:attributes options) 
                                  :ground-truth :prediction))
          yhat (fn yhat ^double [datum] (model xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          0.75 y yhat (:data options))
          _ (println true-train-summary)
          est-functional (taiga/functional model)
          est-dual (z/dual (z/linear-part est-functional))
          est-translation (z/translation est-functional)]
      (println "tru:" true-translation)
      (println "est:" est-translation)
      (println "true:\n" (z/pprint-str (into [] true-dual) 32))
      (println "est:\n" (z/pprint-str (into [] est-dual) 32))
      (test/is (z/approximately== 
                 ulps0 true-translation est-translation))
      (test/is (z/doubles-approximately== 
                 ulps1 true-dual est-dual))
      (test/is (z/maps-approximately== 
                 ulps2
                 true-train-summary
                 train-summary)))))
;;----------------------------------------------------------------
(test/deftest affine-q75
  (test0 coax-q75
         2.0e13 5.0e11 1.0e12
         62.39658552896441
         (double-array [0.64401413936869])
         {:rmean -70.97300609577452
          :rmse 157.2250903172707
          :rmad 91.05459057038878,
          :rmqr 55.56808752250152,
          :rmrq 74.09078336333536}))
;;----------------------------------------------------------------
(test/deftest affine-l1 
  (test0 coax-l1
         5.0e11 5.0e10 1.0e11
         81.4822474169362181 
         (double-array [0.5601805512094196])
         {:rmean -7.694427436042407,
          :rmse 120.3293287452608,
          :rmad 74.72311764947104,
          :rmqr 70.87590393144984,
          :rmrq 94.50120524193312}))
;;----------------------------------------------------------------
(test/deftest affine-l2 
  (test0 taiga/affine-l2
         1.0e3 5.0e4 5.0e7
         147.4753885237055044
         (double-array [0.4851784236769233])
         {:rmean 6.108458892370852e-14,
          :rmse 113.6213303526584,
          :rmad 77.34747450550505,
          :rmqr 77.34747450550505,
          :rmrq 103.1299660073401}))
;;----------------------------------------------------------------
(test/deftest affine-l2-regression 
  (test0 taiga/affine-l2-regression
         1.0e1 1.0e0 2.0e3
         147.4753885237055044
         (double-array [0.4851784236769233])
         {:rmean 6.108458892370852e-14,
          :rmse 113.6213303526584,
          :rmad 77.34747450550505,
          :rmqr 77.34747450550508,
          :rmrq 103.1299660073401}))
;;----------------------------------------------------------------
