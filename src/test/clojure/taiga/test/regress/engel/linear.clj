(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-17"
      :doc "Engel data linear (no intercept) regression models." }
    
    taiga.test.regress.engel.linear
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.defs :as defs]
            [taiga.test.regress.engel.engel :as engel]))
;; mvn -Dtest=taiga.test.regress.engel.linear clojure:test > engel-linear.txt
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
              true-slope
              true-train-summary]
  (z/seconds 
    nss
    (let [options (assoc (engel/options)
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
                         :start [0.0])
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
          est-slope (aget 
                      (doubles (z/dual (taiga/functional model)))
                      0)]
      (println "true:\n" true-slope)
      (println "est:\n" est-slope)
      (test/is (z/approximately== ulps0 true-slope est-slope)
               (print-str "not ==" true-slope est-slope))
      (test/is (z/maps-approximately== ulps1 
                                       true-train-summary
                                       train-summary)))))
;;----------------------------------------------------------------
(test/deftest linear-l2-regression 
  (test0 taiga/linear-l2-regression
         1.0e0 5.0e1
         0.6026217251973054
         {:rmean 32.09051058237403
          :rmse 132.8245731300036
          :rmad 88.15108166877985
          :rmqr 104.1963369599669
          :rmrq 138.9284492799558}))
;;----------------------------------------------------------------
(test/deftest linear-l2 
  (test0 taiga/linear-l2
         1.0e5 5.0e6
         0.6026217251973054
         {:rmean 32.09051058237403
          :rmse 132.8245731300036
          :rmad 88.15108166877985
          :rmqr 104.1963369599669
          :rmrq 138.9284492799558}))
;;----------------------------------------------------------------
(test/deftest linear-l1 
  (test0 coax-l1
         2.0e10 2.0e12
         0.6464302339825654
         {:rmean -10.95016839667965
          :rmse 141.4569873221206
          :rmad 80.4106304656008
          :rmqr 74.93554626726096
          :rmrq 99.91406168968129}))
;;----------------------------------------------------------------
(test/deftest linear-q75
  (test0 coax-q75
         2.0e11 5.0e12
         0.71416589335193
         {:rmean -77.49862784418038
          :rmse 181.6377487598629
          :rmad 96.95840454564059
          :rmqr 58.20909062355041
          :rmrq 77.61212083140055}))
;;----------------------------------------------------------------
