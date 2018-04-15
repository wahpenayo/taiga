(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-14"
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
(defn- test0 [^clojure.lang.IFn fit
              ^double true-slope
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
                         :huber-epsilon 1.0e-2
                         :quantile-p 0.5
                         :start
                         [0.0]
                         :gradient-check (z/print-writer System/out))
          model (fit options)
          _ (defs/edn-test model (defs/linear-edn-file nss))
          y (:ground-truth (:attributes options))
          xbindings (into (sorted-map)
                          (dissoc (:attributes options) 
                                  :ground-truth :prediction))
          yhat (fn yhat ^double [datum] (model xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          est-slope (aget 
                      (doubles (z/dual (taiga/functional model)))
                      0)]
      (println "true:\n" true-slope)
      (println "est:\n" est-slope)
      (test/is (z/approximately== 1.0e-4 true-slope est-slope)
               (print-str "not ==" true-slope est-slope))
      #_(z/mapc 
          (fn [^double x0 ^double x1]
            (test/is (z/approximately== 1.0e-4 x0 x1))) 
          true-train-summary
          train-summary))))
;;----------------------------------------------------------------
(test/deftest linear-l1 
  (test0 taiga/linear-l1
         (double 0.6464302)
         [-10.95017 141.457 80.41063]))
;;----------------------------------------------------------------
#_(test/deftest linear-l2 
    (test0 taiga/linear-l2
           147.4754
           (double-array [0.4852])
           [-1.2743E-8 113.6213 77.3475]))
;;----------------------------------------------------------------
#_(test/deftest linear-l2-regression 
    (test0 taiga/linear-l2-regression
           147.4754
           (double-array [0.4852])
           [-1.2743E-8 113.6213 77.3475]))
;;----------------------------------------------------------------
