(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-17"
      :doc "Engel data constant regression models." }
    
    taiga.test.regress.stackloss.constant
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.defs :as defs]
            [taiga.test.regress.stackloss.stackloss :as stackloss]))
;; mvn -Dtest=taiga.test.regress.stackloss.constant clojure:test > stackloss-constant.txt
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
                    (stackloss/options)
                    :attributes {:ground-truth stackloss/stackloss
                                 :prediction stackloss/predicted-stackloss}
                    :embedding (z/linear-embedding "stackloss" [])
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
  (test0 
    taiga/affine-l2-regression
    1.0e0 5.0e1
    17.52380952380953
    {:rmean -1.691148874377866e-15
     :rmse 9.926487162752505
     :rmad 7.505668934240363
     :rmqr 7.505668934240362
     :rmrq 10.00755857898715}))
;;----------------------------------------------------------------
(test/deftest constant-l2 
  (test0 
    taiga/affine-l2
    1.0e2 5.0e3
    17.52380952380953
    {:rmean -1.691148874377866e-15
     :rmse 9.926487162752505
     :rmad 7.505668934240363
     :rmqr 7.505668934240362
     :rmrq 10.00755857898715}))
;;----------------------------------------------------------------
(test/deftest constant-l1 
  (test0 
    coax-l1
    2.0e10 1.0e11
    15
    {:rmean 2.523809523809524
     :rmse 10.24230256850294
     :rmad 6.904761904761905
     :rmqr 8.166666666666666
     :rmrq 10.88888888888889}))
;;----------------------------------------------------------------
(test/deftest constant-q75
   (test0 
     coax-q75
     1.0e3 5.0e3
      19
     {:rmean -1.476190476190476
      :rmse 10.03565073696199
      :rmad 8.047619047619047
      :rmqr 7.309523809523809
      :rmrq 9.746031746031745}))
;;----------------------------------------------------------------
