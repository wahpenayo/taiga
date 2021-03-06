(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-14"
      :doc "Engel data constant regression models." }
    
    taiga.test.regress.engel.constant
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.defs :as defs]
            [taiga.test.regress.engel.engel :as engel]))
;; mvn -Dtest=taiga.test.regress.engel.constant clojure:test > engel-constant.txt
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
              ulps0 ulps1
              true-translation
              true-train-summary]
  (z/seconds 
    nss
    (let [options (assoc
                    (engel/options)
                    :attributes {:ground-truth engel/foodexp
                                 :prediction engel/predicted-foodexp}
                    :embedding (z/linear-embedding "engel" [])
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
                    :start [0])
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
          est-functional (taiga/functional model)
          est-translation (z/translation est-functional)]
      (println "tru:" true-translation)
      (println "est:" est-translation)
      (test/is (z/approximately== 
                 ulps0 true-translation est-translation))
      (test/is (z/maps-approximately== 
                 ulps1 true-train-summary train-summary)))))
;;----------------------------------------------------------------
(test/deftest constant-l2-regression 
    (test0 taiga/affine-l2-regression
           1.0e0 2.0e3
         624.1501113133555
           {:rmean 3.388636888820791e-14
            :rmse 275.8681638504783
            :rmad 200.4251399540668
            :rmqr 200.4251399540668
            :rmrq 267.2335199387558}))
;;----------------------------------------------------------------
(test/deftest constant-l2 
  (test0 taiga/affine-l2
         1.0e0 2.0e3
           624.1501113133555
         {:rmean 3.388636888820791e-14
          :rmse 275.8681638504783
          :rmad 200.4251399540668
          :rmqr 200.4251399540668
          :rmrq 267.2335199387558}))
;;----------------------------------------------------------------
(test/deftest constant-l1 
   (test0 coax-l1
          1.0e0 2.0e3
           582.54125094185
          {:rmean 41.60886037150559
           :rmse 278.9884246481381
           :rmad 196.92790017903
           :rmqr 217.7323303647828
           :rmrq 290.3097738197104}))
;;----------------------------------------------------------------
(test/deftest constant-q75
  (test0 coax-q75
         1.0e13 5.0e13
         743.8814317451735
         {:rmean -119.7313204318179
          :rmse 300.7304988167662
          :rmad 243.0007504635849
          :rmqr 183.1350902476759
          :rmrq 244.1801203302346}))
;;----------------------------------------------------------------
