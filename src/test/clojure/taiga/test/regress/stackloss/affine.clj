(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-17"
      :doc "Engel data affine regression models." }
    
    taiga.test.regress.stackloss.affine
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.defs :as defs]
            [taiga.test.regress.stackloss.stackloss :as stackloss]))
;; mvn -Dtest=taiga.test.regress.stackloss.affine clojure:test > stackloss-affine.txt
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
    (let [options (assoc (stackloss/options)
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
                         :start [0 0 0 0])
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
(test/deftest affine-l2-regression 
  (test0 taiga/affine-l2-regression
         1.0e1 1.0e1 2.0e2
         -39.919674420123961
         (double-array 
           [-0.152122519148653 0.715640200485284 1.295286124388573])
         {:rmean -3.26512543854148e-14,
          :rmse 2.918169367439919,
          :rmad 2.366620194259908
          :rmqr 2.366620194259892
          :rmrq 3.155493592346523}))
;;----------------------------------------------------------------
(test/deftest affine-l2 
  (test0 taiga/affine-l2
         1.0e9 1.0e9 2.0e8
           -39.919674420123961
           (double-array 
             [-0.152122519148653 0.715640200485284 1.295286124388573])
           {:rmean -3.26512543854148e-14,
            :rmse 2.918169367439919,
            :rmad 2.366620194259908
            :rmqr 2.366620194259892
            :rmrq 3.155493592346523}))
;;----------------------------------------------------------------
(test/deftest affine-l1 
  (test0 coax-l1
         5.0e12 5.0e12 2.0e12
         -39.68985507246379285
         (double-array 
           [-0.06086956521739127 0.83188405797101350 0.57391304347826466])
         {:rmean 0.0894409937888208,
          :rmse 3.29116849160106,
          :rmad 2.003864734299517,
          :rmqr 2.048585231193928,
          :rmrq 2.731446974925237}))
;;----------------------------------------------------------------
(test/deftest affine-q75
   (test0 coax-q75
          5.0e13 5.0e13 1.0e14
          -5.418965517241383e01
          (double-array [2.677979366039196e-16 8.706896551724138e-01 9.827586206896549e-01])
          {:rmean -1.63259441707716
           :rmse 3.616745153732757
           :rmad 2.364121510673232,
           :rmqr 1.547824302134652,
           :rmrq 2.063765736179537}))
;;----------------------------------------------------------------
